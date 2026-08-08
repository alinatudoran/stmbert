# Measuring Organizational Constructs in Large-Scale Text

## STM analysis (Stage 1, Steps 1–6)

Companion code for: "Measuring Organizational Constructs in Large-Scale Text:
An interpretive framework combining topic modeling and BERT", *Organization Studies*.

Structural Topic Modeling (STM) analysis of Glassdoor employee reviews, Stage 1 of the
two-stage construct-measurement framework. This is the R script for the STM
operationalization: data cleaning, text preprocessing, threshold/K selection, the
three candidate models and the final **K = 30** model, topic-level outputs, the
document–topic proportions used to build the labeled dataset (Step 6), the
model-stability validation reported in **Appendix 6**, and the temporal (year)
prevalence analysis reported in **Appendix 7 / Figure A7**.

The script is organised by the paper's numbered Steps. (Paper **Step 2** — manual
coding of a subsample — is done by hand, not in code, so the script goes from Step 1
to Step 6.) **Appendix 6, Test 3 / BERTopic** triangulation is a separate Python
notebook (`bertopic_triangulation.ipynb`).

### Files

| File | Purpose |
|------|---------|
| `STM_replication.R` | Cleaned analysis script (run top to bottom). |
| `bertopic_triangulation.ipynb` | BERTopic triangulation for Appendix 6, Test 3 / Panel C. |
| `data/comments.csv` | Raw Glassdoor review file — **not distributed** (user-supplied; see Data availability). |
| `data/labeled_dataset.xlsx` | Manually coded labeled dataset used in Step 6 (657 rows: `doc_id`, `pros`, 13 binary construct columns) — **not distributed** (user-supplied). |

### Running

1. Install the packages listed at the top of `STM_replication.R` (install block, run once).
2. Place the review file at `data/comments.csv` and confirm `data_path` near the top;
   for Step 6, also place the labeled dataset at data/labeled_dataset.xlsx.
3. Run top to bottom, following the paper-Step headers (Step 1 → Step 3 → Step 4 →
   Step 5 → Step 6 → Appendix 6 → Appendix 7). Seeds are fixed; inline `# ...` comments record
   the expected output at each step. The slow parts are spell correction, the two
   `searchK` sweeps, and the `selectModel` / stability model fits.
4. `sessionInfo()` at the end records the package versions used (see below).

### Data availability

**This is a code-only package. The raw review data is not distributed.**

The employee reviews were obtained from Glassdoor and cannot be redistributed under
the platform's terms of service; the free-text reviews are also personal data and are
not shared for privacy reasons. The analysis code is provided in full, together with
derived, non-identifying outputs (topic–word tables, model diagnostics). 

To reproduce, supply your own `data/comments.csv` (semicolon-separated, with a `pros`
review-text column and the metadata fields used below) and set `data_path` in the
script. In Step 5, the **representative-comments export** and the **document–topic
proportions export** include verbatim review text and require this file; they are
flagged inline with a NOTE and can be skipped if only the models and diagnostics are
needed.

## Section map

- **Step 1 — Data collection, description and cleaning** (3.1): dedup, office-role
  job-title filter, focus on the `pros` field. → 45,875 raw, 20,188 after filtering.
- **Step 3 — STM text preprocessing** (3.3): spell correction, lowercasing,
  repeated-word removal, custom stop words, `textProcessor`; minimum
  document-frequency threshold selection (**Table 3**); effective document length.
- **Step 4 — STM estimation & K selection** (3.4): `searchK` broad (K = 5–100) and
  fine (K = 10–40) → **Figures 2 & 3**; fit the three candidate models K = 24, 27, 30.
- **Step 5 — Qualitative interpretation** (3.5): top-word workbooks
  (prob / FREX / lift / score) and top representative comments for the three
  candidates; **document–topic (θ) proportions export** feeding the labeled-dataset
  construction (Step 6). *(Exports review text.)*
- **Step 6 — Labeled dataset split (3.6)**: dominant-label stratified 80/20 split 
  of the labeled dataset (seed 42) → train_data.csv and test_data.csv,
  the inputs to the BERT notebook; construct distribution check (**Table 4**).  
- **Appendix 6 — Model-stability validation** (3.5.2): Test 1 sensitivity to
  document length (**Panel A**) and Test 2 aggregation-based stability (**Panel B**);
  RBO / cosine(β) / θ metrics. *(Test 3 / BERTopic, Panel C, is in `bertopic_triangulation.ipynb`.)*
- **Appendix 7 — Metadata (year) associations** (3.5.4): topic prevalence,
  2023 vs. ≤ 2022 → **Figure A7**.


## Scaling classification with BERT (Stage 2, Steps 7–9)

Companion code for: "Measuring Organizational Constructs in Large-Scale Text:
An interpretive framework combining topic modeling and BERT", *Organization Studies*.

`BERT_multilabel_classification.ipynb` implements Stage 2 of the framework:
fine-tuning BERT-base to classify employee reviews into 12 constructs plus a
residual *Others* category (multi-label classification).

- **Step 7 (Section 3.7)** — BERT tokenization and token-length audit
- **Step 8 (Section 3.8)** — Optuna hyperparameter search, 5-fold multi-label
  stratified cross-validation with class-weighted loss, per-construct threshold
  tuning, final training (Tables 5–7)
- **Step 9 (Section 3.9)** — held-out test-set validation (Table 8) and example
  inference (Figure 4)

### Data

The review texts are not redistributed (Glassdoor terms of service; the reviews
are also personal data under GDPR). Users supply their own labeled data as
`train_data.csv` and `test_data.csv` (columns: id, `pros` text, one
binary indicator per construct). 

### Running

Run the notebook top to bottom. The first cell installs dependencies. The Optuna
search is optional — the selected hyperparameters are hard-coded, so results can
be reproduced without re-running it. Seeds are fixed (42), but GPU fine-tuning is
not bit-for-bit deterministic, so metrics may vary marginally across runs.
Run on a GPU environment (NVIDIA H100 or Google Colab with a T4 GPU).

