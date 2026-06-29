"""
Evaluates topic classification prompts against the human-coded validation
sample, using items where Coder 1 and Coder 2 agree as the gold standard
(their inter-coder kappa on the full set is 0.832).

Runs either the original 12-category scheme or the merged 9-category
scheme (Economy = Economic+Welfare, Cultural Politics = Identity+
Nationalism, Law & Order = Crime+Security), which was adopted after the
merge raised human-human agreement to 0.870 and gave the LLM cleaner
category boundaries to work with.

    export OPENAI_API_KEY=sk-...
    python3.12 eval_topic_prompt.py          # 9-cat scheme (default)
    python3.12 eval_topic_prompt.py --12cat  # original 12-cat scheme
"""

from __future__ import annotations

import argparse
import json
import os
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from pathlib import Path

import numpy as np
import pandas as pd
from openai import OpenAI
from sklearn.metrics import cohen_kappa_score, confusion_matrix

# Paths (repo root = parent of this script's 02_validation/ directory)
BASE    = Path(__file__).resolve().parents[1]
VAL_DIR = BASE / "validation_sample"
CODING  = VAL_DIR / "finished_coding"
OUT_DIR = BASE / "eval_results"
OUT_DIR.mkdir(exist_ok=True)

MASTER_CSV = VAL_DIR / "topic_validation_full_2026-04-27.csv"
C1_CSV     = CODING  / "topic_coder1_assignments.csv"
C2_CSV     = CODING  / "topic_coder2_assignments.csv"
CACHE_JSON_12      = OUT_DIR / "topic_eval_cache.json"            # gpt-4.1 12-cat
CACHE_JSON_9       = OUT_DIR / "topic_eval_cache_9cat.json"       # gpt-4.1 9-cat
CACHE_JSON_9_MINI  = OUT_DIR / "topic_eval_cache_9cat_mini.json"  # gpt-4.1-mini 9-cat

# Model
MODEL       = "gpt-4.1"
BATCH_SIZE  = 20
MAX_WORKERS = 8
MAX_RETRIES = 4

# Category schemes
CATEGORIES_12 = [
    "Crime", "Democracy", "Economic", "Education", "Environment",
    "Identity", "Migration", "Nationalism", "Other", "Security",
    "Technology", "Welfare",
]

CATEGORIES_9 = [
    "Cultural Politics", "Democracy", "Economy", "Education", "Environment",
    "Law & Order", "Migration", "Other", "Technology",
]

# Maps 12-cat gold labels → 9-cat (for evaluating 9-cat LLM predictions)
MERGE_MAP = {
    "Economic":    "Economy",
    "Welfare":     "Economy",
    "Identity":    "Cultural Politics",
    "Nationalism": "Cultural Politics",
    "Crime":       "Law & Order",
    "Security":    "Law & Order",
    # unchanged
    "Democracy":   "Democracy",
    "Education":   "Education",
    "Environment": "Environment",
    "Migration":   "Migration",
    "Other":       "Other",
    "Technology":  "Technology",
}

# Baselines
# 12-cat: old gpt-4o-mini vs agreed human gold
BASELINE_KAPPA_12 = 0.684
BASELINE_AGREE_12 = 0.711
# 9-cat: old gpt-4o-mini with merged labels vs merged gold (simulation)
BASELINE_KAPPA_9  = 0.729   # computed from confusion matrix merge simulation
BASELINE_AGREE_9  = 0.762   # estimated proportional gain

# Prompts

PROMPT_12 = """\
You are classifying tweets by politicians into exactly one topic category (any language).

CATEGORIES
  Crime            – crime, law enforcement, criminal justice, policing, sentencing, prisons,
                     drugs (law enforcement context), law-and-order.
  Democracy        – democratic processes, institutions, electoral politics, parliamentary
                     procedure, party politics, democratic norms, election campaigns, voting
                     rights, media freedom, comments on other parties or politicians, political
                     scandals. IMPORTANT: accusations that a politician or party is corrupt,
                     criminal, or acting illegally = Democracy (democratic accountability),
                     NOT Crime (which covers actual crime policy, not political attacks).
  Economic         – aggregate economic policy: taxation, fiscal policy, trade, business
                     regulation, inflation, wages, GDP, employment levels, budget, pension
                     reform framed in economic terms.
  Education        – education policy, schools, universities, curricula, student debt,
                     educational inequality.
  Environment      – environmental protection, climate change, energy policy, renewable
                     energy, pollution, biodiversity.
  Identity         – social identity issues about people's characteristics: race, gender,
                     LGBTQ+, religion, ethnicity, culture wars, abortion, historical memory,
                     social values debates. IMPORTANT: Identity = debate about who people ARE
                     (characteristics of persons or groups). Do NOT confuse with Nationalism.
  Migration        – immigration, asylum, refugees, border control, integration, deportations.
  Nationalism      – the nation-state's relationship to other nations or supranational bodies:
                     sovereignty, national independence movements, Brexit, anti-EU sentiment,
                     national pride as a political project, Euroscepticism focused on
                     sovereignty (not economics). IMPORTANT: Nationalism = debate about the
                     nation (country vs outside world). Do NOT confuse with Identity (which
                     is about people's characteristics, not state sovereignty).
  Other            – genuinely non-political or non-assignable: greetings, congratulations,
                     condolences, sports, entertainment, personal announcements, promotional
                     content, unclear single-word or hashtag-only tweets. ONLY use Other when
                     no substantive policy topic can be identified.
  Security         – national security, defence, military, terrorism, policing in a security
                     context, intelligence, international conflicts.
  Technology       – technology, digital policy, social media regulation, AI, cybersecurity,
                     media/press policy (except press freedom, which is Democracy).
  Welfare          – services and benefits for individuals: healthcare, social insurance,
                     housing, disability benefits, family policy, childcare, pensions framed
                     as social provision (not economic reform).

KEY DECISION RULES
1. Identity vs Nationalism: Identity = about people's characteristics (race, gender,
   religion, sexuality, ethnicity, abortion). Nationalism = about the nation-state's
   relationship to outside entities (sovereignty, independence, Brexit, anti-EU,
   national pride as political project).
2. Attacking politicians for corruption/criminality = Democracy (democratic
   accountability), NOT Crime (Crime = actual crime policy and law enforcement).
3. Welfare vs Economic: Welfare = services/benefits for individuals (healthcare,
   housing, childcare). Economic = aggregate economy (taxes, trade, GDP, inflation,
   employment levels).
4. Other: use ONLY for greetings, congratulations, condolences, sports/entertainment,
   non-political personal content, or incomprehensible/hashtag-only tweets. Do NOT
   use Other if any substantive policy topic can be identified.

Use sample_ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["sample_id","Category"],...]} (every sample_id once, Category from list above)"""


PROMPT_9 = """\
You are classifying tweets by politicians into exactly one topic category (any language).

CATEGORIES
  Cultural Politics – debates about social identity, values, and the nation's relationship
                      to outside entities. Covers TWO areas:
                      (a) IDENTITY: race, gender, LGBTQ+, religion, ethnicity, culture wars,
                      abortion, historical memory, social values — debates about people's
                      characteristics or identity groups already present in society.
                      (b) NATIONALISM: sovereignty, national independence, Brexit, anti-EU
                      sentiment, national pride as political project, Euroscepticism focused
                      on sovereignty — the nation-state vs. outside entities.
                      IMPORTANT: Migration = policy about controlling immigration flows and
                      borders. Cultural Politics = debates about identity/values/sovereignty.
                      A tweet calling for stricter border controls = Migration. A tweet about
                      how immigrants undermine national culture or identity = Cultural Politics.
                      Expressions of national pride, flag-waving, "our country first" rhetoric,
                      and sovereignty arguments = Cultural Politics, NOT Other.
  Democracy         – democratic processes, institutions, electoral politics, parliamentary
                      procedure, party politics, democratic norms, election campaigns, voting
                      rights, media freedom, comments on other parties or politicians, political
                      scandals, accusations of corruption or criminality against politicians
                      (= democratic accountability, NOT Law & Order). IMPORTANT: campaign
                      slogans, calls to vote, endorsements of parties/politicians, expressions
                      of support or opposition toward political opponents, and any tweet
                      primarily directed at a political actor = Democracy, NOT Other.
  Economy           – ALL economic and social spending policy. Covers TWO areas:
                      (a) ECONOMIC: aggregate economy — taxation, fiscal policy, trade, business
                      regulation, inflation, wages, GDP, employment levels, budget.
                      (b) WELFARE: individual services and benefits — healthcare, social insurance,
                      housing, disability benefits, family policy, childcare, pensions.
                      Use Economy for both aggregate economic policy AND individual welfare services.
  Education         – education policy, schools, universities, curricula, student debt,
                      educational inequality.
  Environment       – environmental protection, climate change, energy policy, renewable
                      energy, pollution, biodiversity. IMPORTANT: if a tweet addresses both
                      environmental/climate concerns AND economic impacts (e.g. green jobs,
                      energy costs), classify as Environment if climate or environmental
                      protection is the primary frame.
  Law & Order       – crime, policing, and national security. Covers TWO areas:
                      (a) CRIME: crime policy, law enforcement, criminal justice, sentencing,
                      prisons, drugs (enforcement context), law-and-order.
                      (b) SECURITY: national security, defence, military, terrorism, policing in
                      a security context, intelligence, international conflicts.
                      Use Law & Order for both domestic crime policy AND national/military security.
                      IMPORTANT: accusations that a politician is corrupt/criminal = Democracy
                      (democratic accountability), NOT Law & Order. Tiebreaker: if the primary
                      subject is a security threat, crime wave, military operation, or policing
                      policy = Law & Order. If the primary subject is what a politician or party
                      did or should do = Democracy.
  Migration         – immigration, asylum, refugees, border control, integration, deportations.
                      Migration = policy about immigration flows, border controls, and asylum
                      processes. Do NOT use for tweets about cultural identity or sovereignty
                      (those = Cultural Politics).
  Other             – genuinely non-political or non-assignable: greetings, congratulations,
                      condolences, sports scores, entertainment, personal life announcements,
                      or tweets that are literally impossible to interpret (single emoji, only
                      a URL, pure gibberish). ONLY use Other when no political or policy content
                      whatsoever can be identified. Do NOT use Other for: campaign slogans,
                      calls to vote, endorsements, attacks on opponents, expressions of national
                      pride, policy criticism (however brief), or any tweet where the author's
                      political stance is identifiable.
  Technology        – technology, digital policy, social media regulation, AI, cybersecurity,
                      media/press policy (except press freedom, which is Democracy).

KEY DECISION RULES
1. Economy covers BOTH aggregate economic policy (taxes, GDP, trade) AND individual
   welfare/social services (healthcare, housing, childcare, pensions). Use Economy for either.
2. Cultural Politics covers BOTH identity debates (race, gender, religion, sexuality,
   abortion, culture wars) AND nationalist/sovereignty debates (Brexit, anti-EU, national
   independence). Use Cultural Politics for either.
3. Migration = immigration flows/borders/asylum. Cultural Politics = identity/values/sovereignty.
   They are distinct: border policy = Migration; cultural threat framing = Cultural Politics.
4. Law & Order covers BOTH domestic crime policy AND national security/military/defence.
   Use Law & Order for either.
5. Accusations of political corruption or criminality = Democracy (democratic accountability).
6. Environment takes priority over Economy when climate/environment is the primary frame,
   even if economic effects are also mentioned.
7. Law & Order vs Democracy tiebreaker: primary subject is a threat/crime/military operation
   = Law & Order; primary subject is what a politician or party did or should do = Democracy.
8. Other: ONLY for personal/entertainment/congratulatory content with zero political substance.
   Campaign slogans, political attacks, national pride, any policy stance = NOT Other.
9. Cultural Politics includes brief expressions of national pride or "our country first"
   rhetoric — do not code these as Other.

Use sample_ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["sample_id","Category"],...]} (every sample_id once, Category from list above)"""


# API helpers

def classify_batch(client: OpenAI, tweets: list[tuple[str, str]],
                   prompt: str, model: str = "gpt-4.1") -> dict[str, str]:
    """Classify a batch of (sample_id, text) pairs. Returns {sample_id: category}."""
    user_content = "sample_id\ttext\n" + "\n".join(
        f"{sid}\t{txt[:500]}" for sid, txt in tweets
    )
    for attempt in range(MAX_RETRIES):
        try:
            # gpt-5 series only supports default temperature (1)
            temp_kwargs = {} if model.startswith("gpt-5") else {"temperature": 0}
            resp = client.chat.completions.create(
                model=model,
                **temp_kwargs,
                response_format={"type": "json_object"},
                messages=[
                    {"role": "system", "content": prompt},
                    {"role": "user",   "content": user_content},
                ],
                timeout=60,
            )
            raw = json.loads(resp.choices[0].message.content)
            return {str(sid): str(cat) for sid, cat in raw.get("labels", [])}
        except Exception as e:
            wait = 2 ** attempt
            if attempt == MAX_RETRIES - 1:
                print(f"\n  [batch error] {e}")
                return {}
            time.sleep(wait)
    return {}


def run_classification(df: pd.DataFrame, prompt: str,
                       cache_path: Path, model: str = "gpt-4.1",
                       batch_size: int = BATCH_SIZE) -> dict[str, str]:
    """Classify all tweets in df. Loads from / saves to cache_path."""
    cache: dict[str, str] = {}
    if cache_path.exists():
        cache = json.loads(cache_path.read_text())
        print(f"Loaded {len(cache)} cached results from {cache_path.name}")

    to_classify = [(str(r.sample_id), r.text) for r in df.itertuples()
                   if str(r.sample_id) not in cache]

    if not to_classify:
        print("All tweets already cached — skipping API calls.")
        return cache

    print(f"Classifying {len(to_classify)} tweets with {model} (batch={batch_size})...")
    client = OpenAI()
    batches = [to_classify[i:i+batch_size] for i in range(0, len(to_classify), batch_size)]

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as pool:
        futures = {pool.submit(classify_batch, client, b, prompt, model): b for b in batches}
        done = 0
        for fut in as_completed(futures):
            cache.update(fut.result())
            done += 1
            print(f"  {done}/{len(batches)} batches done...", end="\r")

    print(f"\nDone. {len(cache)} total scored.")
    cache_path.write_text(json.dumps(cache, indent=2))
    print(f"Cache saved to {cache_path}")
    return cache


# Metrics

def compute_kappa(y_true: list[str], y_pred: list[str],
                  categories: list[str]) -> float:
    return round(cohen_kappa_score(y_true, y_pred, labels=categories), 3)


def per_category_stats(y_true: list[str], y_pred: list[str],
                       categories: list[str]) -> pd.DataFrame:
    rows = []
    for cat in categories:
        tp = sum(t == cat and p == cat for t, p in zip(y_true, y_pred))
        fp = sum(t != cat and p == cat for t, p in zip(y_true, y_pred))
        fn = sum(t == cat and p != cat for t, p in zip(y_true, y_pred))
        n  = sum(t == cat for t in y_true)
        prec = tp / (tp + fp) if (tp + fp) > 0 else 0.0
        rec  = tp / (tp + fn) if (tp + fn) > 0 else 0.0
        f1   = 2 * prec * rec / (prec + rec) if (prec + rec) > 0 else 0.0
        rows.append(dict(category=cat, n=n, precision=round(prec,3),
                         recall=round(rec,3), f1=round(f1,3)))
    return pd.DataFrame(rows).set_index("category")


def confusion_worst(y_true: list[str], y_pred: list[str],
                    categories: list[str], n_worst: int = 4) -> None:
    cm = confusion_matrix(y_true, y_pred, labels=categories)

    errors = []
    for i, true_cat in enumerate(categories):
        for j, pred_cat in enumerate(categories):
            if i != j and cm[i, j] > 0:
                errors.append((cm[i, j], true_cat, pred_cat))
    errors.sort(reverse=True)

    print(f"\nTop {n_worst} confusion pairs (true → predicted):")
    for count, true_cat, pred_cat in errors[:n_worst]:
        print(f"  {true_cat:18} → {pred_cat:18}  ({count} cases)")


# Main

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--12cat", dest="use_12cat", action="store_true",
                        help="Evaluate 12-category scheme (default: 9-category)")
    parser.add_argument("--model", default="gpt-4.1",
                        help="Model to evaluate (default: gpt-4.1)")
    parser.add_argument("--batch-size", type=int, default=BATCH_SIZE,
                        help=f"Tweets per API call (default: {BATCH_SIZE})")
    args = parser.parse_args()

    use_9cat   = not args.use_12cat
    model      = args.model
    batch_size = args.batch_size
    scheme   = f"9-cat ({model})" if use_9cat else f"12-cat ({model})"
    CATEGORIES  = CATEGORIES_9 if use_9cat else CATEGORIES_12
    PROMPT      = PROMPT_9     if use_9cat else PROMPT_12
    # cache file per model+scheme+batch_size
    model_slug = model.replace("/", "_").replace(".", "_")
    bs_tag = f"_bs{batch_size}" if batch_size != BATCH_SIZE else ""
    if use_9cat:
        CACHE_JSON = OUT_DIR / f"topic_eval_cache_9cat_{model_slug}{bs_tag}.json"
    else:
        CACHE_JSON = OUT_DIR / f"topic_eval_cache_12cat_{model_slug}{bs_tag}.json"
    BL_KAPPA    = BASELINE_KAPPA_9  if use_9cat else BASELINE_KAPPA_12
    BL_AGREE    = BASELINE_AGREE_9  if use_9cat else BASELINE_AGREE_12

    # Load data
    master = pd.read_csv(MASTER_CSV)
    c1 = pd.read_csv(C1_CSV)
    c2 = pd.read_csv(C2_CSV)

    master.columns = master.columns.str.strip()
    c1.columns     = c1.columns.str.strip()
    c2.columns     = c2.columns.str.strip()

    # c2 uses 'notes' as the topic label column
    c1 = c1.rename(columns={"coder_topic": "c1_topic"})
    c2 = c2.rename(columns={"notes": "c2_topic"})

    c1["sample_id"] = c1["sample_id"].astype(int)
    c2["sample_id"] = c2["sample_id"].astype(int)

    df = master.merge(
        c1[["sample_id", "c1_topic"]], on="sample_id", how="left"
    ).merge(
        c2[["sample_id", "c2_topic"]], on="sample_id", how="left"
    )

    # Gold standard: C1 ∩ C2 agreement
    df["c1_topic"] = df["c1_topic"].str.strip()
    df["c2_topic"] = df["c2_topic"].str.strip()
    gold = df[(df["c1_topic"].notna()) & (df["c2_topic"].notna()) &
              (df["c1_topic"] == df["c2_topic"])].copy()
    gold["gold_12"] = gold["c1_topic"]

    # Restrict to valid 12-cat labels
    gold = gold[gold["gold_12"].isin(CATEGORIES_12)].copy()

    if use_9cat:
        gold["gold"] = gold["gold_12"].map(MERGE_MAP)
        gold = gold[gold["gold"].isin(CATEGORIES_9)].copy()
    else:
        gold["gold"] = gold["gold_12"]

    print(f"\n{scheme} evaluation:")
    print(f"Validation set: {len(df)} total, {len(gold)} with C1=C2 agreement")
    print("Gold standard distribution:")
    print(gold["gold"].value_counts().to_string())
    print()

    # Classify
    cache = run_classification(gold, PROMPT, CACHE_JSON, model=model,
                               batch_size=batch_size)
    gold["llm_new"] = gold["sample_id"].astype(str).map(cache)
    missing = gold["llm_new"].isna().sum()
    if missing > 0:
        print(f"WARNING: {missing} tweets unclassified")

    gold_scored = gold[gold["llm_new"].notna()].copy()
    gold_scored["llm_new"] = gold_scored["llm_new"].str.strip()
    # Clamp to valid categories
    gold_scored = gold_scored[gold_scored["llm_new"].isin(CATEGORIES)].copy()

    y_true = gold_scored["gold"].tolist()
    y_pred = gold_scored["llm_new"].tolist()
    n = len(y_true)

    # Report
    kappa = compute_kappa(y_true, y_pred, CATEGORIES)
    pct_agree = round(sum(t == p for t, p in zip(y_true, y_pred)) / n, 3)

    print("=" * 60)
    print(f"  NEW gpt-4.1 ({scheme}) vs agreed human gold (n={n})")
    print("=" * 60)
    print(f"  Cohen's κ       : {kappa}   (baseline old LLM: {BL_KAPPA})")
    print(f"  % exact agree   : {pct_agree:.1%}   (baseline old LLM: {BL_AGREE:.1%})")
    delta_k = round(kappa - BL_KAPPA, 3)
    delta_a = round(pct_agree - BL_AGREE, 3)
    print(f"  Δκ vs baseline  : {delta_k:+.3f}")
    print(f"  Δ% agree        : {delta_a:+.3f}")

    print("\nPer-category breakdown (gold n / precision / recall / F1):")
    stats = per_category_stats(y_true, y_pred, CATEGORIES)
    print(stats.to_string())

    confusion_worst(y_true, y_pred, CATEGORIES, n_worst=8)

    # Also compare old LLM (12-cat) to same gold
    if "topic" in gold_scored.columns:
        if use_9cat:
            gold_scored["topic_merged"] = gold_scored["topic"].map(MERGE_MAP)
            old_in_gold = gold_scored[gold_scored["topic_merged"].isin(CATEGORIES_9)]
            y_old  = old_in_gold["topic_merged"].tolist()
        else:
            old_in_gold = gold_scored[gold_scored["topic"].isin(CATEGORIES_12)]
            y_old  = old_in_gold["topic"].tolist()
        if len(old_in_gold) >= 10:
            y_gold_sub = old_in_gold["gold"].tolist()
            kappa_old  = compute_kappa(y_gold_sub, y_old, CATEGORIES)
            agree_old  = round(sum(t == p for t, p in zip(y_gold_sub, y_old)) / len(y_gold_sub), 3)
            print(f"\nOld LLM ({scheme}) vs same gold subset (n={len(y_gold_sub)}): "
                  f"κ={kappa_old}  agree={agree_old:.1%}")


if __name__ == "__main__":
    main()
