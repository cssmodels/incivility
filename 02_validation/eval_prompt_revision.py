"""
Tests candidate incivility prompts against the human-coded validation
sample, before committing to one for the full-corpus run.

    export OPENAI_API_KEY=sk-...
    python3.12 eval_prompt_revision.py --prompt v2
    python3.12 eval_prompt_revision.py --model gpt-4o --prompt v2

Prints raw and population-reweighted kappa for the chosen prompt/model
and saves per-tweet scores to eval_results/scores_<variant>_<model>.csv.
"""

from __future__ import annotations

import argparse
import json
import os
import time
from pathlib import Path

import numpy as np
import pandas as pd
from openai import OpenAI
from sklearn.metrics import cohen_kappa_score

# Paths (repo root = parent of this script's 02_validation/ directory)
REPO_ROOT = Path(__file__).resolve().parents[1]
BASE = REPO_ROOT / "validation_sample"
CODING = BASE / "finished_coding"
OUT = REPO_ROOT / "eval_results"
OUT.mkdir(exist_ok=True)

# Prompts
PROMPTS = {

"v1": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

Incivility = violation of respectful democratic discourse, incl.: insults/name-calling/ridicule; contempt or derogatory labels; profanity (targeted or not); delegitimizing opponents; threats, intimidation, or endorsing violence.

Scale:
0 = civil/neutral.
1 = mild snide or derisive tone, mild insult, or profanity as emphasis.
2 = clear insult, derogatory label, targeted profanity, strong contempt.
3 = dehumanization, threats/intimidation, violence endorsement, explicit delegitimization.

If unsure, choose the LOWER score.

Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v2": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

IMPORTANT: Score the SPEAKER'S OWN language only.
If a tweet reports, quotes, or condemns hateful/violent speech by others,
score the speaker's framing — not the content being reported.

Incivility = hostile, contemptuous, or demeaning language in democratic discourse.
Policy criticism and strong disagreement are NOT incivility.

Scale:
0 = civil or neutral. Criticism of policies, parties, or decisions is NOT incivility.
1 = mild derision, sarcasm, or partisan snark; sharp criticism without direct personal insult.
2 = direct contemptuous label or insult aimed at a named person or identifiable group;
    derogatory nickname used as attack (e.g. calling someone a crook, a liar, corrupt by name).
    Requires: a specific human target + language that demeans rather than merely criticises.
3 = dehumanization; explicit threats or endorsement of violence against a person;
    calling for someone's removal from democracy (not just from office).
    Mere exaggerated rhetoric does NOT qualify.

Key distinctions:
- "The government's policy is a disaster" → 0 (policy criticism)
- "How can the NDP keep failing Canadians like this?" → 1 (partisan snark, no named target)
- "These are the crooks – @Name" → 2 (direct label at named individual)
- "You are a lying traitor and deserve what's coming" → 3 (threat + dehumanization)

If unsure, choose the LOWER score.
Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v3": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

IMPORTANT RULES:
1. Score the SPEAKER'S OWN language only — not content they quote or report.
2. Political criticism, disagreement, and partisan rhetoric are NOT incivility (score 0).
3. When uncertain between two scores, always choose the LOWER one.

Scale:
0 = Civil or neutral. Includes: policy criticism, factual claims, event reporting,
    partisan advocacy, expressions of disagreement, or rhetoric about abstract groups.
1 = Mild personal edge: snark, sarcasm, condescension, or mild insult without
    direct attack on a named individual. ("Yet another broken promise from the left.")
2 = Direct personal attack: a named or clearly identified person/group is called
    something contemptuous — liar, crook, corrupt, traitor, incompetent, stupid, etc.
    Must be directed AT a specific target, not just rhetoric about policy or parties.
3 = Severe: explicit threat of physical harm, dehumanizing language (animals, vermin,
    filth), endorsing or celebrating violence against a person. Rare.

Examples at each level:
  0: "The minister's approach to housing will hurt families."
  0: "Here is what the prime minister said about tax policy [quote]."
  1: "Another day, another broken promise. Classic Labour."
  1: "This government has no idea what it's doing."
  2: "The PM is a pathological liar and a disgrace."
  2: "These are nothing but corrupt criminals. @Name"
  3: "He deserves a bullet. Traitor."

Use ids exactly as shown. Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v4": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

Score the SPEAKER'S OWN words only. If they quote or report someone else's hateful/violent
language, score how THEY frame it — not the quoted content.

Incivility = contemptuous or demeaning language directed at people in political discourse.
Policy criticism, disagreement, and partisan advocacy are NOT incivility.

Scale:
0 = Civil or neutral. Policy criticism, reporting events, disagreement, advocacy.
1 = Mild snark or derision — implies the target is foolish, dishonest, or incompetent,
    but stops short of a direct demeaning label. Includes rhetorical questions,
    sarcastic commentary, or mocking tone without explicit name-calling.
2 = Explicit contemptuous characterization — the speaker directly labels a person
    or group as corrupt, lying, criminal, stupid, evil, a disgrace, etc.
    The insult/label is the speaker's own assertion, not a quote or ironic observation.
    Target can be named or unnamed (e.g. "this government", "these politicians").
3 = Dehumanization, explicit threats, or endorsing/celebrating violence. Rare.
    Exaggerated political rhetoric does NOT qualify.

Calibration guide — typical score-1 vs score-2 contrast:
  1: "Another broken promise. Shocking, truly shocking." (sarcasm, no direct label)
  1: "How does the PM sleep at night after this?" (rhetorical contempt, no label)
  1: "Typical. This government couldn't organise a raffle." (mockery, no direct insult)
  2: "The PM is a pathological liar." (direct label: liar)
  2: "These MPs are crooks, plain and simple." (direct label: crooks)
  2: "A corrupt, incompetent disgrace of a minister." (stacked contemptuous labels)
  3: "Traitors like him deserve a bullet." (violence endorsement)

If unsure, choose the LOWER score.
Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v5": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

CRITICAL: Score the SPEAKER'S OWN language. Tweets that REPORT or CONDEMN
hateful speech by others should be scored on the speaker's framing only.

0 = No incivility. Criticism of policy, parties, decisions, or outcomes is NOT incivility,
    even if strongly worded.
1 = Mild incivility: the speaker's tone is derisive, contemptuous, or mocking,
    but they do not apply a direct demeaning label to a person or group.
2 = Moderate incivility: the speaker directly and explicitly characterizes a politician
    or group with a contemptuous label (liar, criminal, corrupt, disgrace, traitor, etc.).
    The label is the speaker's own claim — not quoted from someone else.
3 = Severe incivility: dehumanizing language (subhuman, vermin, filth), explicit
    threat of physical harm, or endorsement of violence against a person.

Worked examples (use these to calibrate):
  "The housing policy will hurt millions." → 0
    [criticism of policy outcome, no personal attack]
  "Migrants are raping our daughters" (quoted in condemnation) → 0
    [speaker is condemning hateful speech, not expressing it]
  "Yet another U-turn. Truly a masterclass in incompetence." → 1
    [contemptuous tone, implies incompetence but no direct label]
  "How can anyone still trust these people after this?" → 1
    [rhetorical disgust, no explicit characterization]
  "He is a liar and everyone knows it." → 2
    [direct label: liar, speaker's own assertion]
  "These are corrupt criminals who belong in jail." → 2
    [direct labels: corrupt, criminal]
  "He's a disgusting traitor and should be shot." → 3
    [dehumanizing + violence endorsement]

Score distribution guidance: expect roughly 60-70% score-0,
15-20% score-1, 10-15% score-2, 1-3% score-3 in a typical political corpus.

When unsure, choose the LOWER score.
Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v6": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

Score the SPEAKER'S OWN language. If the tweet reports or quotes someone else's hateful
or violent speech, score the speaker's framing — not the quoted content.

Incivility = language that demeans, insults, or expresses contempt toward people
in political discourse. Policy criticism — even fierce criticism — is NOT incivility.

Scale:
0 = Civil or neutral: policy criticism, event reporting, disagreement, advocacy.
1 = Mild: derision, sarcasm, or mockery toward a political actor, but without
    a direct demeaning characterization. The speaker implies the target is foolish
    or bad without saying it outright.
2 = Moderate: the speaker directly characterizes a political actor or group
    in demeaning terms — e.g., calls them corrupt, lying, criminal, a disgrace,
    spineless, shameful, evil, a hypocrite, etc. The contempt is expressed, not just implied.
    This includes strong contemptuous adjectives ("disgusting behaviour by this minister")
    not just explicit nouns ("liar").
3 = Severe: dehumanization, explicit threat of physical harm, endorsing violence.
    Rare — do not apply to exaggerated political rhetoric.

Boundary guidance:
  Score 1 if the tweet expresses contempt through tone but the language itself
    could describe a poor decision or bad outcome without personalizing it.
  Score 2 if the tweet's language personalises the contempt — the words themselves
    characterize a person/group as bad, corrupt, shameful, or contemptible.

Use your best judgment; do not systematically favour either the higher or lower score.
Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"v7": """\
Task: Score POLITICAL INCIVILITY in each tweet (0-3, any language).

IMPORTANT: Score the SPEAKER'S OWN language only.
If a tweet reports, quotes, or condemns hateful or violent speech by others,
score the speaker's framing — not the content being described.

Incivility = hostile, contemptuous, or demeaning language directed at people
in political discourse. Policy criticism and disagreement are NOT incivility.

Scale:
0 = Civil or neutral. Policy/decision criticism, event reporting, disagreement,
    partisan advocacy. Strong language about outcomes or policies is fine here.
1 = Mild incivility. The tone is derisive, contemptuous, or mocking,
    but stops short of a direct demeaning label — snark, sarcasm, rhetorical contempt.
2 = Moderate incivility. The speaker directly labels a politician or group with
    a contemptuous characterization: corrupt, liar, criminal, disgrace, incompetent,
    coward, hypocrite, thief, traitor, etc. The target may be named or unnamed
    (e.g. "this government", "these MPs", "the left").
    Key test: would the target feel personally attacked and demeaned?
3 = Severe incivility. Dehumanization, explicit threats of violence, or endorsement
    of violence against a person. Mere political hyperbole does NOT qualify.

Key examples:
- "The government's policy will harm millions." → 0
- "Here is what the minister said [quote of bad speech]" → 0 (speaker is neutral)
- "Another broken promise. Classic." → 1 (sarcasm, no direct label)
- "How can people still vote for this shambles of a government?" → 1 (contemptuous tone)
- "The PM is a liar and a coward." → 2 (direct labels)
- "These people are nothing but corrupt crooks." → 2 (direct label, unnamed group)
- "He deserves to be shot, traitor." → 3

If genuinely unsure between 1 and 2, choose 1.
Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

"cb": """\
You are coding political incivility in tweets by politicians. Code only the author's communicative posture, not whether you agree with the claim or whether the claim is true.

Assign one score per tweet: 0, 1, 2, or 3.

SCALE

0 = Civil
No incivility. The tweet may criticize policies, decisions, events, parties, or performance, but it does not use insults, contemptuous language, profanity, derogatory labels, or delegitimizing attacks.
Examples: policy criticism, factual claims, announcements, disagreement, condolences, praise, campaign updates.

1 = Mildly uncivil
Dismissive, sarcastic, mocking, condescending, or mildly contemptuous language, but no direct strong attack. Also code 1 for profanity used only as emphasis rather than as a targeted insult.
Examples: "predictably," "what a surprise," "they should be ashamed," "this is crap," "another brilliant idea from…"

2 = Very uncivil
Direct attack on a person, party, institution, or group. Includes explicit insults, derogatory labels, accusations of lying, corruption, betrayal, malice, incompetence, criminality, hypocrisy, or deliberate deception; targeted profanity; and delegitimizing opponents as enemies, traitors, criminals, racists, extremists, etc.
Examples: "liar," "corrupt," "traitor," "racist," "crooks," "goons," "idiots," "they are destroying the country," "this party is a criminal organization."

3 = Extremely uncivil
Severe norm violation: dehumanization, explicit hate speech, threats, intimidation, calls for violence, endorsement/celebration of harm, wishes of death/destruction, or categorical exclusion of groups from society/politics.
Examples: "parasites," "vermin," "they should be killed," "wipe them out," "nuke them," "they have no place in our society," explicit racist/religious hate, calls for expulsion of an entire group.

DECISION RULES

1. Code the highest level of incivility present. One strongly uncivil phrase is enough.
2. Strong criticism is not automatically uncivil. "The policy failed" = 0. "The minister is incompetent" = 2.
3. Profanity always raises the score: non-targeted profanity = at least 1; targeted profanity/abuse = at least 2.
4. Sarcasm/mockery without direct attack = usually 1. Sarcasm used to make a direct attack = 2.
5. Attacks on institutions, parties, media, courts, governments, or social groups count the same as attacks on individuals.
6. Derogatory language toward ethnic, religious, gender, sexual, immigrant, or other social groups is at least 2; hate speech, dehumanization, or calls for exclusion/violence are 3.
7. If the tweet quotes or reports uncivil language by others in order to condemn it, code the author's posture, not the quoted content. Usually this is 0, but note that graphic quoted abuse was present.
8. If the tweet reports threats or violence without endorsing them, code the reporting posture, usually 0.
9. If the tweet is unclear, only a link, only a hashtag with no clear incivility, or impossible to evaluate, code 0.
10. When genuinely unsure between two scores, choose the lower score.

Use ids exactly as shown (strings). Output ONLY JSON:
{"labels":[["id",score],...]} (every id once)""",

}

# Helpers
BATCH_SIZE = 20   # tweets per API call — overridden by --batch_size

# Models that don't support temperature or system role
_NO_TEMP_PREFIXES = ("o1", "o3", "o4", "gpt-5")

def _no_temperature(model: str) -> bool:
    base = model.split(":")[0]
    return any(base.startswith(p) for p in _NO_TEMP_PREFIXES)


def score_batch(client: OpenAI, tweets: list[tuple[str, str]], system_prompt: str,
                model: str) -> dict[str, int]:
    """Score a batch of (id, text) tuples. Returns {id: score}."""
    user_content = "id\ttext\n" + "\n".join(f"{tid}\t{txt[:500]}" for tid, txt in tweets)
    no_temp = _no_temperature(model)
    for attempt in range(3):
        try:
            kwargs: dict = dict(
                model=model,
                response_format={"type": "json_object"},
                timeout=60,
                messages=[
                    {"role": "system", "content": system_prompt},
                    {"role": "user",   "content": user_content},
                ],
            )
            if not no_temp:
                kwargs["temperature"] = 0
            resp = client.chat.completions.create(**kwargs)
            raw = json.loads(resp.choices[0].message.content)
            labels = raw.get("labels", [])
            return {str(tid): int(score) for tid, score in labels}
        except Exception as e:
            if attempt == 2:
                print(f"    [error] {e}")
                return {}
            time.sleep(2 ** attempt)
    return {}


def run_prompt(df: pd.DataFrame, prompt: str, model: str,
               out_path: Path, col: str = "llm_new") -> pd.DataFrame:
    """Score all tweets in df and return df with new column named `col`."""
    if out_path.exists():
        print(f"  Loading cached results: {out_path.name}")
        cached = pd.read_csv(out_path)
        # cache may use "llm_new" or already the right col name
        if "llm_new" in cached.columns and col not in cached.columns:
            cached = cached.rename(columns={"llm_new": col})
        if col in df.columns:
            df = df.drop(columns=[col])
        if col not in cached.columns:
            print(f"  [warn] col '{col}' missing in cache — skipping")
            return df
        return df.merge(cached[["sample_id", col]], on="sample_id", how="left")

    client = OpenAI()
    rows = list(zip(df["sample_id"].astype(str), df["text"].fillna("")))
    results: dict[str, int] = {}

    for i in range(0, len(rows), BATCH_SIZE):
        batch = rows[i : i + BATCH_SIZE]
        scores = score_batch(client, batch, prompt, model)
        results.update(scores)
        if (i // BATCH_SIZE) % 10 == 0:
            print(f"  {i+len(batch)}/{len(rows)} tweets scored...", end="\r")

    print(f"\n  Done. {len(results)}/{len(rows)} scored.")
    df = df.copy()
    df[col] = df["sample_id"].astype(str).map(results)
    df[["sample_id", col]].rename(columns={col: "llm_new"}).to_csv(out_path, index=False)
    return df


# Metrics
POP_W = {0: 0.83 / 0.125, 1: 0.12 / 0.375, 2: 0.045 / 0.375, 3: 0.005 / 0.125}


def wkappa_raw(a, b):
    mask = ~(np.isnan(a) | np.isnan(b))
    if mask.sum() < 10:
        return np.nan, 0
    a, b = a[mask].astype(int), b[mask].astype(int)
    return round(cohen_kappa_score(a, b, weights="quadratic", labels=[0,1,2,3]), 3), int(mask.sum())


def wkappa_pop(llm, human, row_weights):
    mask = ~np.isnan(human)
    if mask.sum() < 10:
        return np.nan, 0
    l, h, w = llm[mask].astype(int), human[mask].astype(int), row_weights[mask]
    cm = np.zeros((4, 4))
    for i, j, wi in zip(l, h, w):
        cm[i, j] += wi
    cm /= cm.sum()
    n = 4
    wmat = np.array([[1 - (i-j)**2 / (n-1)**2 for j in range(n)] for i in range(n)])
    po = (cm * wmat).sum()
    pe = sum(cm[i,:].sum() * cm[:,j].sum() * wmat[i,j]
             for i in range(n) for j in range(n))
    return round((po - pe) / (1 - pe), 3), int(mask.sum())


def binary_stats(llm, human, thresh=2):
    mask = ~np.isnan(human)
    l = (llm[mask] >= thresh).astype(int)
    h = (human[mask] >= thresh).astype(int)
    tp = ((l==1) & (h==1)).sum()
    fp = ((l==1) & (h==0)).sum()
    fn = ((l==0) & (h==1)).sum()
    tn = ((l==0) & (h==0)).sum()
    prec = tp / (tp + fp) if (tp + fp) > 0 else 0
    rec  = tp / (tp + fn) if (tp + fn) > 0 else 0
    f1   = 2 * prec * rec / (prec + rec) if (prec + rec) > 0 else 0
    return dict(precision=round(prec,3), recall=round(rec,3), f1=round(f1,3),
                llm_rate=round(l.mean(),3), human_rate=round(h.mean(),3))


def report(df: pd.DataFrame, score_col: str, label: str):
    df = df[df[score_col].notna()].copy()
    df[score_col] = df[score_col].astype(float)
    df["row_weight"] = df["incivility_llm"].map(POP_W)

    print(f"\n{'='*60}")
    print(f"  {label}  (n={len(df)})")
    print(f"{'='*60}")

    print(f"  Score distribution:")
    for s in [0,1,2,3]:
        orig = (df["incivility_llm"]==s).mean()
        new  = (df[score_col]==s).mean()
        print(f"    {s}: orig={orig:.1%}  new={new:.1%}")

    print(f"\n  vs Coder 1:")
    for cname in ["c1", "c2"]:
        sub = df[df[cname].notna()]
        if len(sub) < 20:
            continue
        k_raw, n = wkappa_raw(sub[score_col].values, sub[cname].values)
        k_pop, _ = wkappa_pop(sub[score_col].values, sub[cname].values,
                              sub["row_weight"].values)
        bst = binary_stats(sub[score_col].values, sub[cname].values)
        print(f"  vs {cname.upper()}: κ_raw={k_raw}  κ_pop={k_pop}  "
              f"prec={bst['precision']}  rec={bst['recall']}  F1={bst['f1']}  "
              f"(LLM≥2: {bst['llm_rate']:.1%}, Human≥2: {bst['human_rate']:.1%})  n={n}")

    # Language breakdown
    print(f"\n  Pop-reweighted κ by language family (vs C1):")
    for lf in ["English", "Germanic", "Romance", "Other"]:
        sub = df[(df["lang_family"]==lf) & df["c1"].notna()]
        if len(sub) < 10:
            continue
        k_pop, n = wkappa_pop(sub[score_col].values, sub["c1"].values,
                              sub["row_weight"].values)
        print(f"    {lf:<12}: κ={k_pop}  (n={n})")


# Sweep config
# Combinations to run with --sweep. Edit freely.
SWEEP = [
    # (prompt_key, model)  — gpt-4.1-mini and gpt-4.1 already cached, will load from disk
    ("cb",  "gpt-4.1-mini"),
    ("cb",  "gpt-4.1"),
    ("cb",  "gpt-5-mini"),
    ("cb",  "gpt-5"),
    ("cb",  "gpt-5.5"),
    ("v2",  "gpt-4.1"),
    ("v2",  "gpt-5-mini"),
    ("cb",  "o4-mini"),
    # fine-tuned UvA models
    ("v1",  "ft:gpt-4o-mini-2024-07-18:university-of-amsterdam::AQGtyViO"),
    ("cb",  "ft:gpt-4o-mini-2024-07-18:university-of-amsterdam::AQGtyViO"),
    ("v2",  "ft:gpt-4o-mini-2024-07-18:university-of-amsterdam:negative-tone-1:B4UYPRcD"),
    ("cb",  "ft:gpt-4o-mini-2024-07-18:university-of-amsterdam:negative-tone-1:B4UYPRcD"),
]


# Main
def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--prompt", default="cb", choices=list(PROMPTS),
                        help="Prompt variant to test (default: cb)")
    parser.add_argument("--model", default="gpt-4.1-mini",
                        help="Model to use (any OpenAI model ID)")
    parser.add_argument("--all", action="store_true",
                        help="Run all prompt variants on the chosen model")
    parser.add_argument("--sweep", action="store_true",
                        help="Run the full SWEEP matrix of (prompt, model) combinations")
    parser.add_argument("--batch_size", type=int, default=None,
                        help="Tweets per API call (default: 20). Use a different value to test bs=50 etc.")
    args = parser.parse_args()

    if args.batch_size:
        import sys
        sys.modules[__name__]  # keep linter happy
        globals()["BATCH_SIZE"] = args.batch_size

    # Load data
    master = pd.read_csv(BASE / "validation_sample_full_2026-04-26.csv")
    c1 = pd.read_csv(CODING / "coder1_assignment_v2.csv")
    c2 = pd.read_csv(CODING / "coder2_assignments_v2.csv")
    c3 = pd.read_csv(CODING / "coder3_turkish_assignments.csv")
    c4 = pd.read_csv(CODING / "coder4_greek_assignments.csv")

    for df in [c1, c2, c3, c4]:
        df.columns = df.columns.str.strip()
        df["sample_id"] = df["sample_id"].astype(int)
        df["coder_score"] = df["coder_score"].astype(float)

    df = master \
        .merge(c1[["sample_id","coder_score"]].rename(columns={"coder_score":"c1"}), on="sample_id", how="left") \
        .merge(c2[["sample_id","coder_score"]].rename(columns={"coder_score":"c2"}), on="sample_id", how="left") \
        .merge(c3[["sample_id","coder_score"]].rename(columns={"coder_score":"c3"}), on="sample_id", how="left") \
        .merge(c4[["sample_id","coder_score"]].rename(columns={"coder_score":"c4"}), on="sample_id", how="left")

    # Baseline: original scores
    print("\n--- BASELINE (original v1 scores from full corpus run) ---")
    df["orig"] = df["incivility_llm"].astype(float)
    report(df, "orig", "v1 original (gpt-4o-mini-2024-07-18)")

    # Build list of (variant, model) pairs to run
    if args.sweep:
        pairs = SWEEP
    elif args.all:
        pairs = [(v, args.model) for v in PROMPTS.keys()]
    else:
        pairs = [(args.prompt, args.model)]

    # Summary table (printed at end)
    summary_rows: list[dict] = []

    for variant, model in pairs:
        prompt_text = PROMPTS[variant]
        safe_model = model.replace(":", "_").replace("/", "_").replace("-", "_")
        bs_suffix = f"_bs{BATCH_SIZE}" if BATCH_SIZE != 20 else ""
        tag = f"{variant}_{safe_model}{bs_suffix}"
        out_path = OUT / f"scores_{tag}.csv"
        col = f"llm_{tag}"
        print(f"\n--- {variant} / {model} ---")
        df = run_prompt(df, prompt_text, model, out_path, col=col)
        report(df, col, f"{variant} ({model})")

        # Collect top-line summary
        sub = df[df[col].notna() & df["c1"].notna()].copy()
        sub["row_weight"] = sub["incivility_llm"].map(POP_W)
        if len(sub) >= 20:
            k_pop, n = wkappa_pop(sub[col].values, sub["c1"].values,
                                  sub["row_weight"].values)
            bst = binary_stats(sub[col].values, sub["c1"].values)
            summary_rows.append(dict(
                variant=variant, model=model,
                kappa_pop=k_pop, precision=bst["precision"],
                recall=bst["recall"], f1=bst["f1"],
                llm_rate=bst["llm_rate"], n=n,
            ))

    # Print leaderboard
    if len(summary_rows) > 1:
        print("\n" + "="*80)
        print("  LEADERBOARD (sorted by κ_pop vs C1)")
        print("="*80)
        sdf = pd.DataFrame(summary_rows).sort_values("kappa_pop", ascending=False)
        for _, r in sdf.iterrows():
            print(f"  κ={r.kappa_pop:.3f}  prec={r.precision:.3f}  rec={r.recall:.3f}  "
                  f"F1={r.f1:.3f}  LLM≥2={r.llm_rate:.1%}  "
                  f"{r.variant} / {r.model}")
        sdf.to_csv(OUT / "leaderboard.csv", index=False)
        print(f"\n  Saved → {OUT / 'leaderboard.csv'}")


if __name__ == "__main__":
    main()
