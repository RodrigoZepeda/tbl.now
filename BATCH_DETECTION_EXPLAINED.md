---
title: "Batch detection, explained at the whiteboard"
output:
  html_document:
    toc: true
    toc_depth: 2
  pdf_document: default
---

# Batch detection, explained at the whiteboard

**What this is.** A plain-language companion to `BATCH_DETECTION_FOR_FABLE.md`
(the questions) and `BATCH_DETECTION_ANSWERS.md` (Fable's answers). Nothing new
is claimed here; the goal is to make the two documents *usable* — to say what is
actually going on, in foundational statistical language, before any named theorem
shows up.

**The problem, in one breath.** Reports arrive late. Sometimes a recording system
*stalls* and then *dumps* a pile of backlogged reports all at once. We call that a
**batch**. We want to find batches — once with the fitted model, once with no model
at all — without mistaking a batch for a genuine surge in the underlying signal.

**The whole thing in one sentence.**

> **A batch moves items. It does not create them.**

Every detector in this document is a way of testing that single sentence.

Each section below has the same four parts: **Core Intuition** → **Jargon Decoder**
→ **Mathematical Mechanics** → **Bottom Line**.

---

## 1. The Geometry: two clocks, one triangle

### The Core Intuition

Every item carries **two timestamps**: when it *happened* (origin time $t$) and
when it was *recorded* (registration time $r$). The gap $d = r - t$ is the lag.
Only two of the three are free — $r = t + d$ — so the data live on a 2-D lattice
that you can index any way you like.

Draw it. Put origin $t$ across the bottom and lag $d$ up the side. Today is time
$\tau$: you cannot see anything that hasn't been recorded yet, so you only observe
cells with $t + d \le \tau$. **The observable region is a triangle.**

Now the move that makes this whole problem tractable. **A single registration date
$r$ is an anti-diagonal of that triangle** — a line running down-left to up-right,
collecting one cell from each origin. Reports that arrived "today" came from many
different origin days, each with a different lag.

And here is the crux, which took me embarrassingly long to see:

> **On a fixed diagonal, the lag is a deterministic function of the origin.**
> If you know an item was registered at $r$ and originated at $t$, its lag is
> $r - t$. No randomness left.

So "the distribution of lags among today's reports" and "the distribution of
origins among today's reports" are *the same object viewed from two ends*. All four
batch symptoms you'd intuitively look for — big volume, long lags, many distinct
origin dates, a preceding lull — are functionals of **one vector**.

Fable added a structural gift I'd missed: **the triangle never cuts a diagonal in
half.** Every diagonal with $r \le \tau$ is *completely* observed. Truncation
removes whole diagonals, never parts of them. That means diagonal-level statistics
need **no truncation correction whatsoever**.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Right truncation** | We only see items already recorded by today; late ones are simply absent, not "censored at a known value". |
| **Run-off triangle** | The observed region: origin × lag, cut by the diagonal $t + d \le \tau$. Actuaries call it this. |
| **Anti-diagonal** | The set of cells sharing the same $t + d$ — i.e. one registration date. |
| **Censoring horizon $\tau$** | "Today". The edge of what we can see. |
| **Diagonal profile** | The vector of counts along one registration date, indexed by lag. |

### The Mathematical Mechanics

Let $n_{t,d}$ be the number of items with origin $t$ recorded at lag $d$. Define:

**The diagonal total** — how many reports arrived on day $r$:

$$R_r \;=\; \sum_{t \le r} n_{t,\;r-t}$$

**The diagonal profile** — *who* those reports were, indexed by lag $\delta$:

$$c_r(\delta) \;:=\; n_{r-\delta,\;\delta}, \qquad \delta = 0, 1, \dots, r-1$$

so that $R_r = \sum_\delta c_r(\delta)$. Reading $c_r$ from left to right walks you
backwards through origin dates: $\delta = 0$ is "originated today", $\delta = 5$ is
"originated five days ago".

The four intuitive symptoms, all read off $c_r$:

$$
\underbrace{R_r = \textstyle\sum_\delta c_r(\delta)}_{\text{volume}}
\qquad
\underbrace{\bar{L}_r = \tfrac{1}{R_r}\textstyle\sum_\delta \delta\, c_r(\delta)}_{\text{mean lag}}
\qquad
\underbrace{U_r = \#\{\delta : c_r(\delta) > 0\}}_{\text{how many origin dates}}
$$

and the fourth — a preceding lull — needs the *neighbouring* diagonals
$R_{r-1}, R_{r-2}, \dots$

### The Bottom Line

We are not testing four things. We are testing **one vector per registration date,
plus its neighbours**. This collapse is what turns a vague "look for weird report
days" into a well-posed statistical problem — and because diagonals are never
partially observed, the null distributions we derive will be *exact*, not
asymptotic.

---

## 2. The model — and the hole in it

### The Core Intuition

Our nowcasting model is a **data-generating process indexed by origin, not by
registration**. It says: each origin day $t$ produces $N_t$ items with some smooth
intensity $\lambda_t$; each item independently waits a random lag drawn from a
*single, fixed* lag distribution $g_D$; then it appears.

Note what is *absent*. There is **no term anywhere in the model indexed by $r$**.
The model has no vocabulary for "something happened to the recording system on
Tuesday". It literally cannot express a batch.

That sounds like a weakness. It is actually the entire basis of detection.

Because if items independently pick lags from a fixed distribution, then the
expected number arriving on day $r$ is a **convolution** — a blurred, smeared copy
of the intensity. And blurring can only ever *remove* sharp structure, never add
it. So:

> **The model guarantees the registration axis is smooth.**
> Any sharp, high-frequency structure on the report axis is something the model
> is structurally incapable of producing.

That's the hole, and the batch lives in it.

One honest correction from Fable, which I'd been sloppy about: this is a statement
about the **mean**, not the data. Real data have Poisson noise, which is white
(flat across all frequencies). So the testable version is: *standardised residuals
should have a flat spectrum; excess narrow-band power is evidence against the
model.*

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Poisson colouring / thinning** | If a Poisson number of items each independently pick a lag, the counts in each lag bin are independent Poissons. Splitting a Poisson keeps everything Poisson and independent. |
| **Gamma frailty** | A per-origin random multiplier $\Lambda_t$ (mean 1) that makes some days busier than the smooth trend predicts — this is what turns Poisson into negative binomial. |
| **Convolution $(\lambda * g_D)$** | "Blur the intensity curve by the lag distribution." Each origin's mass gets spread forward in time according to $g_D$. |
| **Fourier transform $\hat g_D(\omega)$** | A recipe for asking "how much wiggle at frequency $\omega$ survives the blurring?" For a probability distribution, the answer is always "at most all of it". |
| **Low-pass filter** | An operation that keeps slow wiggles and kills fast ones. Blurring is one. |
| **Band-limited** | Containing no fast wiggles. |

### The Mathematical Mechanics

**The model, spelled out.** Conditional on the intensity,

$$N_t \sim \operatorname{Poisson}(\lambda_t), \qquad D \sim g_D(\cdot), \qquad \log \lambda_t = \text{smooth function of } t$$

Poisson colouring then gives the cell-level law — and note that the two indices
separate completely:

$$n_{t,d} \;\sim\; \operatorname{Poisson}\bigl(\underbrace{\lambda_t}_{\text{depends only on origin}} \times \underbrace{g_D(d)}_{\text{depends only on lag}}\bigr), \qquad \text{independent across all } (t,d)$$

**Sum along a diagonal** to get the expected reports on day $r$:

$$\mu_r \;:=\; \mathbb{E}[R_r] \;=\; \sum_{t \le r} \lambda_t\, g_D(r - t) \;=\; (\lambda * g_D)(r)$$

Read that right-hand side slowly: *"for each past origin day $t$, take how many
items it made ($\lambda_t$), multiply by the chance an item waits exactly $r - t$
days ($g_D(r-t)$), and add up."* That is all a convolution is.

**Why blurring can't sharpen.** Take Fourier transforms — $\hat\mu(\omega) =
\hat\lambda(\omega)\,\hat g_D(\omega)$ — and note that because $g_D$ is a
probability distribution, $\hat g_D(\omega) = \mathbb{E}[e^{-i\omega D}]$ is an
average of unit-length complex numbers, so

$$|\hat g_D(\omega)| \;\le\; 1 \qquad\Longrightarrow\qquad |\hat\mu(\omega)| \;\le\; |\hat\lambda(\omega)|$$

Every frequency is attenuated. Fable made the attenuation explicit:

$$1 - |\hat g_D(\omega)|^2 \;=\; \sum_{d < d'} 2\, g_D(d)\, g_D(d')\,\bigl(1 - \cos\omega(d - d')\bigr)$$

Every pair of distinct lags in the support contributes a strictly positive term (as
long as $\omega(d - d')$ isn't a multiple of $2\pi$). **A lag distribution spread
over two or more values strictly destroys high-frequency content.** Only a
degenerate lag distribution — everybody waits exactly the same number of days —
preserves it.

**The testable version.** Standardise:

$$z_r \;=\; \frac{R_r - \hat\mu_r}{\sqrt{\widehat{\operatorname{Var}}\, R_r}}$$

Under the model these have a flat (white) spectrum at level $\approx 1$. Excess
narrow-band power at high $\omega$ — especially where $|\hat g_D(\omega)|$ is small
— is evidence against the model.

### The Bottom Line

The model's blind spot **is** the detector. We aren't hunting for a parameter the
model got wrong; we're hunting for structure of a *kind* the model cannot generate
at all. That's a much stronger position to test from — and it tells us exactly
where to look: **sharp features on the report axis**.

---

## 3. What a batch actually is

### The Core Intuition

A batch is a **transport**, not a creation.

Picture a queue. Items arrive at the recording desk when they're ready. Normally the
desk stamps them immediately. Sometimes the desk closes — items pile up — and when
it reopens, the whole pile gets stamped on one day.

Nothing was invented. Nothing vanished. Each item still belongs to the origin day it
always belonged to. The only thing that changed is **when it got stamped**, and it
can only ever get stamped *later*, never earlier.

That gives three properties, and they are the definition:

1. **Identity on the origin axis.** An item's $t$ is untouched.
2. **Monotone on the lag axis.** Its lag only increases: $d' \ge d$.
3. **Mass is conserved.** Items are neither created nor destroyed.

My original draft said the displacement was "measurable with respect to the
registration axis", which Fable correctly flagged as ambiguous — it could mean
"determined by the ideal registration time" or "may depend on the past". The right
condition is the second: **adapted** (non-anticipating). The desk can decide to close
based on what has already happened, but not on what's coming. This matters: FIFO
release satisfies it, and it's precisely what the main theorem needs.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Transport** | Mass moved from one report day to another, with nothing created or lost. |
| **Release kernel $\kappa$** | The rule deciding, for each item, which day it actually gets stamped given when it was ready. |
| **Non-anticipating / adapted** | The stalling rule can depend on the past, but not peek at the future. |
| **FIFO vs. uniform release** | When the desk reopens, does it clear the backlog oldest-first, or in random order? |
| **Pathwise identity** | An equation true for every single realisation, not just on average. The strongest kind. |
| **Ideal registration time $r^\star$** | When an item *would* have been recorded had nothing stalled. |

### The Mathematical Mechanics

Each item has an **ideal** registration time drawn from the clean model:

$$r_i^\star \;=\; t_i + D_i, \qquad D_i \sim g_D$$

The batch mechanism is a rule $\kappa$ that maps it to an **observed** time:

$$r_i \;\sim\; \kappa\bigl(\,\cdot \mid r_i^\star,\; \mathcal{F}_{r_i^\star}\,\bigr), \qquad \text{supported on } \{r \ge r_i^\star\}$$

where $\mathcal{F}$ is the history (adaptedness), and $\kappa$ never touches $t_i$.
The observed lag is therefore

$$d_i \;=\; r_i - t_i \;=\; \underbrace{D_i}_{\text{true lag}} \;+\; \underbrace{(r_i - r_i^\star)}_{\text{how long it sat in the pile } \ge\, 0}$$

**Special case (the simple picture).** Let $H$ be a set of "closed" days. Define the
next-open-day map

$$\varrho(u) \;=\; \min\{\, v \ge u \;:\; v \notin H \,\}, \qquad r_i = \varrho(r_i^\star)$$

If days $\{b-L, \dots, b-1\}$ are all closed and $b$ is open, then day $b$ receives
its own fresh items **plus** everything that piled up over $L$ days. Immediately:

| symptom | why |
|---|---|
| volume spike at $b$ | it absorbs $L+1$ days of arrivals |
| inflated lags at $b$ | held items wait out the whole closure |
| many distinct origin dates at $b$ | the backlog spans all origins that were ready during the hold |
| lull before $b$ | $R_j = 0$ for $j \in H$ |

All four symptoms fall out of **one** mechanism. They are not four independent
clues; they are four views of the same transport.

### The Bottom Line

Defining a batch as *transport* rather than as *"a weirdly big day"* is what buys
everything downstream. Because mass is conserved **pathwise**, we get an exact test
statistic (§4) and a clean way to separate a batch from a genuine surge (§5). Had we
defined a batch as "a spike", we'd have had nothing to exploit.

---

## 4. The conservation law

### The Core Intuition

Here is the payoff, and it is beautiful.

If a batch only *shuffles items around inside a window of days*, then **the total
number of items in that window is completely unaffected**. Every item you'd have
seen in the window, you still see in the window — just on a different day.

So: draw a box around days $\{r-k, \dots, r\}$. Count everything inside the box. That
count **does not know** whether a batch happened. It has exactly the distribution it
would have had under the clean model.

A genuine surge is different. A surge *creates* items. The box count goes up.

That is the entire discrimination, and it needs no approximation.

I had guessed at this — I proposed the statistic $\Delta = (\text{excess at } r) -
(\text{deficit before } r)$ and sketched a Gaussian argument for why its mean is
zero. Fable's reply: the Gaussian argument can be **thrown away**. The statement is
exact, and $\Delta$ is nothing more exotic than *the box count minus its expected
value*.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Pivot** | A statistic whose distribution is the same no matter what the nuisance (here: the batch) is doing. |
| **Ancillary** | Carries no information about the thing you're trying to ignore. $\Delta$ is blind to transport. |
| **Cumulants** | Mean, variance, skewness, ... For a Poisson, *all* of them equal the mean — a fingerprint. |
| **Exact null** | The reference distribution is right for finite samples, not merely as $n \to \infty$. |
| **Within-window transport** | A batch whose backlog is picked up and released inside the box. |

### The Mathematical Mechanics

Define the **deficit** accumulated in the $k$ days before $r$:

$$W_r(k) \;=\; \sum_{j = r-k}^{r-1} \bigl(\mu_j - R_j\bigr) \qquad \text{(how many reports went missing beforehand)}$$

and the **transport discriminant**:

$$\Delta_r(k) \;=\; \underbrace{\bigl(R_r - \mu_r\bigr)}_{\text{excess on day } r} \;-\; \underbrace{W_r(k)}_{\text{deficit before day } r}$$

Now watch the algebra collapse. Substitute and regroup:

$$
\Delta_r(k)
= (R_r - \mu_r) - \sum_{j=r-k}^{r-1}(\mu_j - R_j)
= \sum_{j=r-k}^{r} R_j \;-\; \sum_{j=r-k}^{r} \mu_j
$$

So, writing $S_r(k)$ for the **window total** and $M_r(k)$ for its null mean:

$$\boxed{\;\Delta_r(k) \;=\; S_r(k) \;-\; M_r(k)\;}$$

$\Delta$ *is* the box count minus its expectation. The "excess minus deficit"
framing was just a disguise.

**The theorem.** For *any* transport whose displacements stay inside the window —
deterministic or random, FIFO or uniform, any closure pattern at all:

$$S_r(k) \;\sim\; \operatorname{Poisson}\bigl(M_r(k)\bigr) \qquad \textbf{exactly}$$

whereas a creation event adding expected mass $\eta$ gives

$$S_r(k) \;\sim\; \operatorname{Poisson}\bigl(M_r(k) + \eta\bigr)$$

*Why it's true, in one line:* transport relabels the report-day of items that
already exist. The number of items in the box is therefore **pathwise** equal to the
number of *ideal* registrations in the box — which is a sum of independent Poisson
cells. Since it holds for every realisation, it holds conditionally on whatever
randomness the closure rule uses, hence unconditionally.

Consequences:

$$\mathbb{E}[\Delta] = 0, \qquad \operatorname{Var}[\Delta] = M, \qquad \text{(indeed all cumulants} = M)$$

for **every** batch strength. And $\Delta$ is the *unique* linear combination of
(excess, deficit) with this property: demanding $a(\eta + m) + b\,m = \eta$ for all
hold strengths $m$ forces $a = 1$, $b = -1$.

**Under negative binomial** the reference law changes (it becomes a sum of
independent NBs, computable by convolution or saddlepoint) but exact ancillarity
survives. You need the covariance, which Fable supplies — note the two pieces:

$$\operatorname{Cov}(R_r, R_{r'}) \;=\; \underbrace{\mathbf{1}\{r = r'\}\sum_{t\le r} m_t(r)}_{\text{Poisson part}} \;+\; \underbrace{\frac{1}{\rho}\sum_{t \le r \wedge r'} m_t(r)\, m_t(r')}_{\text{shared-frailty part}}, \qquad m_t(r) = \lambda_t\, g_D(r-t)$$

Two diagonals are correlated **only** through origins that feed both — so the
correlation vanishes once $|r - r'|$ exceeds the width of the lag support.

**Where exactness leaks.** Two ways, both one-sided and diagnosable by scanning $k$:

- the hold **straddles the window edge** (mass enters or leaves the box);
- the release lands **past $\tau$** — transport plus truncation equals deletion, and
  then $\Delta < 0$.

### The Bottom Line

We have an **exact, finite-sample, distribution-free-in-the-batch** test statistic,
computable from nothing but diagonal totals and an estimate of $\mu$. This is an
unusually strong position for a scan problem, and it means the crudest possible
implementation (§7, a running median) already comes with a rigorous $p$-value.

---

## 5. Batch vs. surge: peeling the onion

### The Core Intuition

We now have two numbers per candidate day: the box count $\Delta$ (blind to
transport, sensitive to creation) and the deficit $W$ (sensitive to transport). Plot
them against each other and the diagnosis reads off directly:

| | **$W \approx 0$** (no preceding lull) | **$W \gg 0$** (a real lull) |
|---|---|---|
| **$\Delta \approx 0$** | nothing happened | **batch** — mass merely moved |
| **$\Delta \gg 0$** | **genuine surge** — mass was created | both at once |
| **$\Delta < 0$** | — | **hold still open**, or the release fell past $\tau$ |

The deeper structure is a **three-layer conditioning argument**, and it's worth
seeing because it explains *why* these particular statistics and not others.

Think of generating a diagonal in three stages, like drawing from an urn:

1. **How many items in the window?** → sensitive to *creation*, immune to transport.
2. **Given that many, how are they spread across the days of the window?** →
   sensitive to *transport*, immune to the overall level.
3. **Given a day's total, what mix of origins/lags does it contain?** → sensitive to
   *lag inflation*.

Each stage conditions away the nuisance of the stage before. That's why each test is
*exactly* the right size regardless of what $\lambda$ and $g_D$ are.

And there's a strong converse. It's not just that conditioning is *allowed* — the
Neyman-structure theorem says any test that has exactly the right size for *all*
$(\lambda, g_D)$ **must** be a conditional test of this form. The construction isn't
one option among many; it's forced.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **$\alpha$-similar test** | A test whose false-positive rate is exactly $\alpha$ no matter what the unknown nuisance parameters are. |
| **Neyman structure** | The theorem saying such tests must be built by conditioning on a sufficient statistic for the nuisance. |
| **Complete sufficient statistic** | A summary that squeezes out everything the data say about the nuisance, leaving no residual information. |
| **Cut (Barndorff-Nielsen)** | A clean split of a model into "the part that knows about $\mu$" and "the part that knows about the shape", which don't interfere. |
| **Locally most powerful (LMP)** | The best statistic for detecting a *small* departure in a specified direction. |
| **Least favourable alternative** | The hardest-to-detect version of the alternative — the one you design against so you're safe everywhere. |
| **Score** | The derivative of the log-likelihood at the null; the natural "which direction is the data pulling?" statistic. |

### The Mathematical Mechanics

**Layer 1 — the window total.** As in §4:

$$S_r(k) \sim \operatorname{Poisson}(M_r(k)) \text{ under transport}; \qquad \operatorname{Poisson}(M + \eta) \text{ under creation}$$

Test creation with a one-sided Poisson tail on $S$. It is UMP among $S$-measurable
tests.

**Layer 2 — the allocation, conditional on the total.** Given $S = n$, the clean
model says those $n$ items land across the window's days multinomially, in
proportion to the null means:

$$\bigl(R_j\bigr)_{j \in \mathcal{W}} \;\Big|\; S = n \;\sim\; \operatorname{Multinomial}\!\left(n, \; \left(\frac{\mu_j}{M}\right)_{j \in \mathcal{W}}\right)$$

A transport moves items to *later* $j$. The locally most powerful statistic against a
hold with shape $w_j \ge 0$ is exactly the weighted deficit:

$$\sum_{j} w_j \bigl(\mu_j - R_j\bigr) \;=\; \text{our } W_r(k) \text{ with weights}$$

**Why $W$ isn't just "reasonable" but efficient.** Write the two scores at the null —
$\eta$ for creation, $m$ for hold magnitude:

$$U_\eta \;=\; \frac{R_r}{\mu_r} - 1, \qquad\qquad U_m \;=\; U_\eta \;+\; \sum_{j < r} w_j\left(1 - \frac{R_j}{\mu_j}\right)$$

Read the second equation: **the transport score is the creation score plus the
weighted relative deficit.** So the part of the transport signal that is *orthogonal*
to creation is precisely the deficit. $W$ is the efficient direction, not a heuristic.

**The least favourable hold.** Which closure pattern is hardest to see? Minimising
the deficit's information $\sum_j w_j^2/\mu_j$ subject to $\sum_j w_j = 1$ gives

$$w_j \;=\; \frac{\mu_j}{\sum_{i}\mu_i} \qquad \text{— \textbf{proportional thinning}}$$

i.e. "the desk slows down uniformly" rather than closing outright. Against *that*,
the optimal statistic is the plain **unweighted** $W_r(k)$. Convenient: the naive
choice is the right one against the worst case.

**Layer 3 — the shape, conditional on the day's total.** See §6.

### The Bottom Line

Report the pair $(\Delta_r(k),\, W_r(k))$ and you have a principled, exactly-sized
classifier for *batch vs. surge vs. ongoing hold* — the single most practically
important output of this whole enterprise. And the three-layer structure tells us
what a full implementation should compute, in what order, and why nothing else would
do.

---

## 6. Inside a single day — and why `surprise()` fails

### The Core Intuition

Now zoom into one report day and ask *who* is in it.

Under the clean model, today's reports are a mixture of origins: mostly recent ones
(short lags are common), a few old ones. A batch tilts that mixture toward **old
origins** — the backlog. So the diagonal profile leans right.

Our package already has a detector called `surprise()`. It flags an *individual*
report whose lag is in the extreme tail of the fitted lag distribution. Fable's
verdict is blunt, and I think correct:

> **`surprise()` is looking at the wrong end of the problem.**

Here's why, and it's the most useful statistical idea in the whole document. A batch
does **not** produce one item with a shocking lag. It produces **many items with
mildly inflated lags** — a whole day's backlog, each sitting maybe 3–5 days longer
than it should.

Statisticians call this the **dense–weak** regime: many small departures. The
opposite is **sparse–strong**: one or two huge departures. And these two regimes have
*different optimal detectors*. A max-type rule (flag the worst item) is the right tool
for sparse–strong. It is provably the *wrong* tool for dense–weak, where you want to
*add up* lots of small evidence.

`surprise()` is a max-type rule. A batch is dense–weak. Hence the mismatch.

What should we use? Two answers:

- If you expect the *whole* diagonal to be affected: just take the **mean lag**. It's
  the optimal (score) statistic, and it's trivial.
- If only *part* of the diagonal is affected (short holds, one stalled site, FIFO
  recovery days): use **higher criticism**, which automatically adapts across the
  whole sparse↔dense spectrum at essentially no cost.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **PIT (probability integral transform)** | Convert each observation to "what fraction of the null distribution lies below it". Under the null these are uniform on $[0,1]$. |
| **Randomised PIT** | The fix for discrete data (lags are integers): add a uniform jitter inside each atom so the result is *exactly* uniform. Required, not optional. |
| **Dense–weak / sparse–strong** | Many tiny signals vs. a few huge ones. Different regimes need different detectors. |
| **Higher criticism (HC)** | A statistic that scans all significance thresholds at once and reports the most surprising one — adaptive across regimes. |
| **Berk–Jones** | A close cousin of HC with exact finite-sample calibration. |
| **Detection boundary** | The exact frontier of signal strength below which *no* test can work; used to certify a statistic as rate-optimal. |
| **Exponential tilt** | A one-parameter way of nudging a distribution toward larger values: reweight by $e^{\theta\delta}$. |

### The Mathematical Mechanics

**The conditional shape.** Given the day's total, the clean model says the profile is
multinomial over lags:

$$c_r \;\big|\; R_r \;\sim\; \operatorname{Multinomial}\bigl(R_r,\; q_r\bigr), \qquad q_r(\delta) \;\propto\; \lambda_{r-\delta}\, g_D(\delta)$$

Unpack $q_r(\delta)$: *"the chance a report on day $r$ came from $\delta$ days ago is
proportional to how many items that day produced ($\lambda_{r-\delta}$) times the
chance of waiting exactly $\delta$ days ($g_D(\delta)$)."* It is $g_D$ **re-weighted
by how busy each past origin was**.

**Why mean lag is optimal.** Model the batch's tilt as an exponential family nudge:

$$q_\theta(\delta) \;\propto\; q_r(\delta)\, e^{\theta \delta}, \qquad \theta > 0 \text{ means "leaning toward older origins"}$$

The score at $\theta = 0$ — the derivative of the log-likelihood — is

$$\left.\frac{\partial}{\partial\theta}\right|_{\theta=0} \log q_\theta \;\;\propto\;\; \sum_\delta \delta\; c_r(\delta) \;=\; R_r \cdot \bar{L}_r$$

So **the mean lag on the diagonal is the locally most powerful statistic** against the
batch tilt. Not a heuristic; the score.

**The PIT and the two regimes.** For each item with lag $D_i$, define the randomised PIT

$$U_i \;=\; \bigl(1 - G_D(D_i)\bigr) \;+\; V_i\, g_D(D_i), \qquad V_i \sim \operatorname{Uniform}(0,1)$$

Exactly uniform under the null; **small $U_i$ means a long lag**. Now, with $n$ items
and a non-null fraction $n^{-\beta}$:

| statistic | rate-optimal where | our situation |
|---|---|---|
| max / per-item threshold — **this is `surprise()`** | sparse only: $\beta > 3/4$ | ✗ wrong regime |
| mean lag / sum-type | dense only: $\beta < 1/2$ | ✓ if whole diagonal hit |
| **higher criticism / Berk–Jones** | the *entire* boundary, adaptively | ✓ always |

A batch has $\beta \approx 0$ (nearly every item on the diagonal is affected) with
small per-item shifts. For `surprise()` to fire, it needs a single item with
$1 - G_D(d_i) \lesssim 1/n$ — which a pile of *mildly* inflated lags simply never
produces. That is the precise, quantitative version of "surprise is weak".

Higher criticism, on sorted PITs $U_{(1)} \le \dots \le U_{(n)}$:

$$\mathrm{HC} \;=\; \max_{1 \le i \le n/2} \; \sqrt{n}\;\frac{\;i/n \;-\; U_{(i)}\;}{\sqrt{U_{(i)}\bigl(1 - U_{(i)}\bigr)}}$$

In words: *"for every possible cutoff $i$, compare how many small PITs you observed
($i/n$) with how many you'd expect ($U_{(i)}$), standardise, and take the worst."* It
tries every threshold so you don't have to choose one.

### The Bottom Line

This changes an existing package function. `surprise()` is fine for what it was built
for — spotting a single implausibly delayed report. **It should not be relied on to
find batches**, and we now know exactly why, and exactly what to add: mean lag as the
cheap default, higher criticism when the batch may be partial.

---

## 7. Detecting with no model at all

### The Core Intuition

Everything above used $\mu_r$, which needs $\lambda$ and $g_D$ — i.e. a fitted model.
For exploratory work we want detectors that need **neither**.

Three routes, and they line up beautifully with the three layers of §5:

- **Route B3 — totals only (crudest, most robust).** Estimate $\mu_r$ with a
  **running median** of neighbouring $R$ values. Then run the $(\Delta, W)$ machinery
  as before. *Why a median and not a mean?* Because a batch corrupts a run of
  consecutive days — deficits then a spike — and a mean would drag the baseline
  toward the very anomaly we're hunting. A median is immune, provided the window is
  wide enough to outvote the episode.
- **Route B2 — shapes only.** Compare today's lag profile with its neighbours' by a
  **permutation test**. The astonishing fact (Fable's Proposition 12): this is
  *exactly* valid whenever $\log\lambda$ is merely **linear** on the window. Not
  constant — *linear*. Any smooth trend is locally linear, so this is a far weaker
  requirement than it first appears.
- **Route B1 — the joint structure (most powerful, most delicate).** The clean model
  implies **lag is independent of origin**. Truncation muddies this (long lags can only
  be seen for old origins), but the residual structure is a well-studied one:
  independence *given* that the item was observable at all. Test it, locally.

One trap Fable caught in my draft: you cannot localise the independence test to a
*single diagonal*, because on a diagonal the lag is a deterministic function of the
origin (§1). There's no association left to measure. You must smooth **across**
neighbouring report days.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Quasi-independence** | "Independent, except for the fact that we could only ever have seen certain combinations." Independence *within the observable region*. |
| **Risk set $\mathcal{R}(x)$** | All items that *could have been observed* with lag $x$ — the fair comparison group. |
| **Lynden-Bell / Efron–Petrosian NPMLE** | A recipe for estimating the lag distribution from truncated data, with no parametric assumption. |
| **Tsai's statistic** | A truncation-corrected rank correlation between lag and origin; zero under quasi-independence. |
| **Exchangeability** | Within a fair comparison group, every member was equally likely to be the one that "failed" — which licenses permutation. |
| **Permutation test** | Reshuffle labels many times to build the null by brute force. Exact, no asymptotics. |
| **Hampel filter** | A running-median-based outlier filter. |
| **Martingale transform** | A running sum of "observed minus predicted-from-the-past" terms; automatically mean-zero, giving free standard errors. |

### The Mathematical Mechanics

**B3 — robust totals.** Fable quantified exactly how wide the median window must be.
A batch episode corrupts $L + 1$ consecutive points ($L$ deficits, one spike). A
running median of width $w$ is **exactly unbiased at every clean point** provided
fewer than half its window is corrupted:

$$w \;\ge\; 2L + 3 \qquad\Longrightarrow\qquad \text{median bias} = 0 \text{ outside the episode}$$

whereas the running **mean** carries bias $O(\text{batch mass}/w)$ across a whole
$2w$-neighbourhood. Then standardise, with the negative-binomial variance:

$$z_r \;=\; \frac{R_r - \hat\mu_r}{\sqrt{\hat\mu_r\bigl(1 + \hat\mu_r / \hat\rho\bigr)}}$$

and apply $(\Delta, W)$ from §4 using the model-free $\hat\mu$. The estimation error
enters only additively, so the logic of the conservation law survives.

**B2 — the permutation miracle.** Suppose $\log\lambda_t$ is linear on the window:
$\lambda_t = e^{a + \zeta t}$. Substitute into the conditional shape from §6:

$$q_r(\delta) \;\propto\; \lambda_{r - \delta}\, g_D(\delta) \;=\; e^{a + \zeta(r-\delta)}\, g_D(\delta) \;\propto\; e^{-\zeta \delta}\, g_D(\delta)$$

The $e^{\zeta r}$ factored out and **cancelled in the normalisation**. So

$$q_r(\delta) \quad\text{does not depend on } r.$$

Every diagonal in the window has the *same* lag profile — an exponentially tilted
$g_D$ — whatever $\lambda$ and $g_D$ and $\zeta$ happen to be. Therefore, conditional
on the diagonal totals, the day-labels are an **exchangeable assignment**, and *any*
$k$-sample permutation test is **exactly distribution-free**. Recommended: a one-sided
Wilcoxon of today's lags against the pooled neighbours (the rank analogue of the
optimal mean-lag score from §6).

Two caveats, both real:

- Only **curvature** in $\log\lambda$ biases it. With $\log\lambda_t \supset
  \tfrac{\kappa}{2}t^2$, the size distortion is $O\bigl(n\,(\kappa k\,
  \sigma_\delta^2)^2\bigr)$ — negligible unless curvature is visible at the window
  scale.
- **Frailty breaks exactness**: neighbouring diagonals share origins, hence share
  $\Lambda_t$. Fix with *block* permutation, using blocks separated by more than the
  width of $g_D$'s support.

**B1 — quasi-independence, localised.** Recode each item as

$$X_i = D_i \;\;(\text{lag}), \qquad Y_i = \tau - t_i \;\;(\text{the largest lag it could possibly have shown})$$

We observe an item iff $X_i \le Y_i$. Under the clean model $X \perp Y$ *before*
truncation — that's exactly quasi-independence. The fair comparison group for an item
failing at lag $x$ is its **risk set**

$$\mathcal{R}(x) \;=\; \{\, i \;:\; X_i \le x \le Y_i \,\}$$

Under the null, *which* member of $\mathcal{R}(x)$ is the one to register at lag $x$
is **uniform**. That single fact gives an **exact** null by permuting within risk
sets — no asymptotics needed. (Fable confirms this answers my "exact rather than
asymptotic?" question affirmatively; the martingale representation is then just a
convenience for a fast normal approximation.)

Localise with a kernel in the *registration* coordinate, weighting each item by how
close its report day is to the candidate $b$:

$$T_b \;=\; \frac{\sum_i \omega_i \bigl(Y_i - \bar{Y}_{\mathcal{R}(X_i)}\bigr)}{\sqrt{\sum_i \omega_i^2\, V_{\mathcal{R}(X_i)}}}, \qquad \omega_i = K_h\bigl(t_i + X_i - b\bigr)$$

Numerator: *"was this item's truncation bound unusually large compared to its risk-set
peers?"* Batch items have both $X_i = b - t_i$ and $Y_i = \tau - t_i$ decreasing in
$t_i$ — so they're **positively associated**, and the scan is one-sided.

**Which to use.** They probe the three layers of §5:

| route | probes | exactness | blind to |
|---|---|---|---|
| **B3** median + $(\Delta, W)$ | totals | exact given $\hat\mu$ | shapes |
| **B2** block permutation | shapes | exact under log-linear $\lambda$ | pure volume anomalies |
| **B1** localised risk-set scan | the joint | exact by permutation | nothing (but costly) |

Default: **B3 + B2**. Reach for B1 when items are few enough to permute risk sets.

### The Bottom Line

We get a genuinely assumption-light exploratory tool — a running median and a Poisson
tail — that nonetheless carries an **exact** null. That is a lot of rigour for very
little machinery, and it's where any implementation should start.

---

## 8. The calendar confounder

### The Core Intuition

Weekends. Holidays. Nightly cron jobs.

A recording desk that is *always* closed on Sundays produces, every single week, all
four batch symptoms: a lull, then a spike, with inflated lags and many origin dates.
This is a **scheduled transport**. It is not what we're hunting.

So the real null is not "the clean model" — it is "**the clean model plus a periodic
schedule**". A batch is an *irregular* transport, an excursion **relative to the
schedule**.

Two separate questions follow, and it's important not to confuse them:

1. **Can we even tell the schedule apart from the rest of the model?** (identifiability)
2. **Given that we can, how do we estimate it without the batches poisoning it?**
   (robustness)

I had guessed you'd need the lag distribution to be "wider than the period $P$".
Fable's answer is sharper and much weaker: **you only need two adjacent lags in the
support**.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Aliasing** | Two different sets of parameters producing identical predictions — you cannot tell them apart from data. |
| **Character (of $\mathbb{Z}_P$)** | A pure oscillation $\omega^r$ with $\omega^P = 1$; these are exactly the ways a period-$P$ effect can hide inside the other parameters. |
| **Parity class** | Even lags vs. odd lags. |
| **Phase** | Position in the cycle: Monday, Tuesday, ... |

### The Mathematical Mechanics

Enrich the model with a report-day multiplier $\psi_r$, assumed $P$-periodic:

$$\mathbb{E}\,c_r(\delta) \;=\; \psi_r \; \lambda_{r-\delta}\; g_D(\delta)$$

Which reparameterisations leave **every** cell mean unchanged? Fable's answer: exactly
the ones built from a $P$-th root of unity $\omega$:

$$\psi_r \mapsto \psi_r\, \omega^{-r}, \qquad \lambda_t \mapsto \lambda_t\, \omega^{t}, \qquad g_D(\delta) \mapsto g_D(\delta)\, \omega^{\delta}, \qquad \omega^P = 1$$

Check it: the three factors contribute $\omega^{-r} \cdot \omega^{r - \delta} \cdot
\omega^{\delta} = \omega^0 = 1$. The mean is untouched. This is the alias.

Now kill the aliases using the fact that $g_D$ is a *real, non-negative probability
mass function*:

- Any **complex** $\omega$ would make $g_D(\delta)\,\omega^\delta$ complex. Dead.
- That leaves $\omega = -1$ (needs $P$ even), which multiplies $g_D(\delta)$ by
  $(-1)^\delta$. This stays non-negative **only if the support of $g_D$ sits entirely
  in one parity class** — all-even or all-odd lags.

Hence:

> **If $g_D$ puts mass on two *adjacent* lags $\delta$ and $\delta + 1$, the periodic
> component is identified.** Whatever $P$ is.

**Estimating it robustly.** Identifiability doesn't stop batches from contaminating
your estimate of the schedule. Fix: estimate each phase's effect by the **median across
cycles** (of $z_r$, or the shape tilt, or $W$). This recovers $\psi_{\text{per}}$
exactly provided each phase is hit by irregular batches in **fewer than half of its
cycles** — the precise replacement for my hand-wave. Then run every detector on the
phase-adjusted array.

Note this stays fine even when a hold spans a whole period ($L \ge P$): locally it
mimics a schedule shift, but across many cycles it still contaminates each phase less
than half the time.

### The Bottom Line

**Do the calendar adjustment before anything else, or every detector will fire every
Monday.** And the condition for it to be possible is mild enough to essentially always
hold in practice (any lag distribution with mass on two consecutive days).

---

## 9. Why you can't just add a "report-date effect"

### The Core Intuition

The obvious idea: if the model lacks a report-day term, add one. Let $\psi_r$ be a free
multiplier and estimate it. A batch is then just a spike in $\psi$.

**This does not work**, and the reason is a classical one in disguise.

Write everything on the log scale. Our three indices are origin $t$, lag $d$, and
report day $r = t + d$. A model with an effect on each is

$$\log \mathbb{E}\,c_r(\delta) \;=\; \underbrace{a_t}_{\text{origin}} + \underbrace{b_d}_{\text{lag}} + \underbrace{c_{t+d}}_{\text{report day}}$$

But $r = t + d$ is a *linear function of the other two*. So the three effects are not
free of each other. This is **exactly** the age–period–cohort problem: origin =
cohort, lag = age, report day = period. It is one of the oldest identification
failures in statistics, and it's been sitting inside our triangle the whole time.

The consequence: you can shift a **linear trend** back and forth between the three
effects without changing a single predicted count. Something has to pin it down.

I proposed **mass conservation** as the pin — the idea that a batch moves mass so the
totals must be preserved. Fable's verdict: **it doesn't work.** One scalar equation
cannot fix a two-dimensional ambiguity.

What *does* work is **sparsity**. A batch isn't a free report-day effect; it's a
report-day effect that is **exactly 1 almost everywhere**. And that's enough.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **APC (age–period–cohort)** | The classic problem: three time indices, one of which is the sum of the other two, so their linear trends are confounded. |
| **Kernel / null space** | The set of parameter changes that leave every prediction identical — the exact shape of what you *cannot* learn. |
| **Kuang–Nielsen–Nielsen** | The standard fix: reparameterise in terms of second differences, which *are* identified. |
| **Toeplitz / convolution constraint** | Our lag effect enters as a blur with fixed shape, not as a free interaction. |
| **Spike-and-slab / horseshoe** | Priors that say "almost all of these are exactly zero, but a few are large" — how you encode sparsity. |

### The Mathematical Mechanics

**The kernel, exactly.** For the map $(a, b, c) \mapsto (a_t + b_d + c_{t+d})$ on the
triangle, Fable shows the null space is **exactly 3-dimensional**:

$$a_t = \alpha + \xi t, \qquad b_d = \beta + \xi d, \qquad c_r = -(\alpha + \beta) - \xi r$$

Verify: $a_t + b_d + c_{t+d} = \alpha + \xi t + \beta + \xi d - (\alpha + \beta) -
\xi(t + d) = 0$. Every prediction unchanged.

Two of those dimensions are harmless levels ($\alpha, \beta$). The dangerous one is
$\xi$ — a **linear tilt** that can slide between origin, lag, and report day. On the
multiplicative scale:

$$\lambda_t \mapsto A\,e^{\xi t}\lambda_t, \qquad g_D(d) \mapsto B\,e^{\xi d} g_D(d), \qquad \psi_r \mapsto (AB)^{-1} e^{-\xi r}\psi_r$$

**Counting constraints, one at a time:**

| constraint | kills | remaining |
|---|---|---|
| "$g_D$ is a probability mass function" | fixes $B$ | $A, \xi$ (**a tilted pmf is still a pmf!**) |
| "$\sum_r \log\psi_r = 0$" | fixes the level | $\xi$ |
| "**mass conservation**" $\sum_r \psi_r \mu_r = \sum_r \mu_r$ | generically fixes $A$ | $\xi$ — **still alive** |
| "$\sum_r r \log\psi_r = 0$" (a *trend* constraint) | fixes $\xi$ | ✓ identified |

So my mass-conservation idea removes the level, not the tilt. **One scalar equation
against a two-dimensional residual family.** Fable's blunt "**No.**"

**What sparsity buys.** Suppose instead $\psi_r = 1$ for all $r$ outside a sparse set.
Any alias must satisfy $(AB)^{-1}e^{-\xi r} = 1$ at every clean $r$. Take **two**
distinct clean report days $r_1 \ne r_2$:

$$e^{-\xi r_1} = e^{-\xi r_2} \;\Longrightarrow\; \xi = 0 \;\Longrightarrow\; AB = 1 \;\xrightarrow{\;\;g_D \text{ is a pmf}\;\;}\; A = B = 1$$

**Two clean days kill the entire kernel.** A *free* report-day effect is not
identified; a *sparse* one is completely identified. That is the rigorous sense in
which "a batch" is estimable but "a general report-day effect" is not.

**One last subtlety, and it's a nice one.** A free $\psi$-spike-with-compensating-dip
and a genuine transport can produce *identical means*. They differ in **higher
moments**: a transport moves *realised* items, so the spike and the deficit are
perfectly coupled given the ideal draws (Theorem 10: $\operatorname{Var} S = M$
exactly), while independent $\psi$-effects would make them independent Poissons with
larger window fluctuation. **Transport vs. free period effect lives in the likelihood,
not in the mean — and $\Delta$ is again the statistic that carries it.**

### The Bottom Line

Don't add a free report-day effect: it's unidentified, and no amount of clever
normalisation involving mass conservation will save it. **Add a *sparse* one** (a
spike-and-slab prior on $\log\psi_r$). Sparsity isn't a convenience or a
regularisation choice here — it is the thing that makes the parameter exist at all.

---

## 10. Watching it live

### The Core Intuition

If we're monitoring a stream, we want an alarm. But a batch is a strange kind of
signal: it goes **down, then up, then back to normal**. A lull, a dump, over.

Standard sequential tools (CUSUM and friends) are built for changes that **persist** —
"the mean shifted and stayed shifted". Their optimality theory simply doesn't cover a
transient, self-cancelling blip. Using them here is a category error.

The right idea is a **matched filter**: we know the *shape* of the signature we're
looking for (a run of $L$ negative residuals then one big positive), so we correlate
the data against that shape, for each plausible $L$, and take the best match.

There's a lovely bonus. The **deficit part *is* persistent** while the desk is closed.
So run **two** detectors:

- a **one-sided CUSUM on the deficit** → fires *during* the hold, giving an **early
  warning** before the dump ever arrives;
- the **matched filter** → confirms at the dump itself, essentially instantly.

Then classify with $(\Delta, W)$ from §5.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **CUSUM / Shiryaev–Roberts** | Classic alarms for a change that *stays*. Wrong tool for a blip — right tool for the ongoing deficit. |
| **Window-limited GLR** | Test every candidate episode of length $\le L_{\max}$ ending now, take the most significant. |
| **Matched filter** | Correlate the data with the known shape of the signal; provably optimal when the shape is known. |
| **ARL (average run length)** | How long you typically wait for a false alarm. Sets the threshold. |
| **Siegmund approximation** | A formula for the false-alarm rate of a scan over many overlapping windows. |
| **SNR** | Signal-to-noise ratio; how loud the signature is relative to Poisson noise. |

### The Mathematical Mechanics

Standardise the residuals against the frozen posterior:

$$z_j \;=\; \frac{R_j - \mu_j}{\sqrt{\mu_j}}$$

For a candidate hold of length $L$ ending at day $b$, the *expected shape* of
$(z_{b-L}, \dots, z_b)$ is: $L$ negative dips, then one big spike carrying all the
backlog:

$$\theta^{(L)} \;\propto\; \Bigl(\,-\sqrt{\mu_{b-L}},\;\dots,\;-\sqrt{\mu_{b-1}},\;\; \tfrac{\sum_{j=b-L}^{b-1}\mu_j}{\sqrt{\mu_b}}\,\Bigr)$$

Monitor the best-matching template:

$$M_b \;=\; \max_{L \le L_{\max}} \; \frac{\bigl\langle \theta^{(L)},\; z_{b-L:b}\bigr\rangle}{\bigl\lVert \theta^{(L)} \bigr\rVert}$$

**How loud is the signal?** Decompose the template's energy:

$$\bigl\lVert\theta^{(L)}\bigr\rVert^2 \;=\; \underbrace{\sum_{j} \mu_j}_{\text{from the } L \text{ dips}} \;+\; \underbrace{\frac{\bigl(\sum_j \mu_j\bigr)^2}{\mu_b}}_{\text{from the single spike}}$$

The **spike term is quadratic** in the backlog and therefore dominates. Practically:
detection at $b$ is essentially **immediate** (delay 0–1 days) once the backlog
exceeds $z_\alpha\sqrt{\mu_b}$. The dips alone accumulate SNR like
$\sqrt{\sum_{j} \mu_j}$ — slower, but available *before* the spike. Hence the
early-warning/confirmation pairing.

**Thresholds.** Asymptotically $M_b$ is the max of a Gaussian field, giving a
Siegmund-type false-alarm rate $\Pr(\max_{b\le T} M_b > u) \approx c\,T\,L_{\max}\,
u\,\phi(u)$. For small counts, skip the Gaussian entirely — Theorem 10 hands us the
*exact* Poisson null of the window statistics, which is a rare luxury in scan
problems. **The overlap constant $c$ is not derived**; Fable flags it as open, and
recommends simulating under the frozen posterior (which we need anyway).

### The Bottom Line

Monitoring needs two alarms, not one, and neither of them is a textbook CUSUM on the
counts. Because the deficit accumulates *while the desk is still shut*, we can often
**warn before the dump lands** — which is the operationally valuable moment.

---

## 11. Many channels, and the retrospective trap

### The Core Intuition

**Channels.** A batch is anomalous "for at least one stratum". Sometimes the whole
system stalls; sometimes one reporting site does. These need *opposite* detectors:
sum the evidence across channels for a system-wide stall, take the max for a single
site. Since we don't know which, use a statistic that adapts across the spectrum —
which is **the same higher-criticism idea as §6**, now applied across channels instead
of across items.

**The retrospective trap.** Here is the uncomfortable part. Everything in §5–§6 used
$\mu_r$ and $q_r$ from a fitted model. If we fit that model to data *containing the
batch*, the model will happily **absorb** it: $\hat\lambda$ inflates near the affected
origins and $\hat g_D$ grows a fat tail to explain the long lags. The batch becomes
part of the null, and the test goes quiet. This is **masking**.

The clean fix is to never let the batch into the fit: freeze the posterior on earlier
data and test forward (**prospective**). That's the setting we should build first.

If we *must* work retrospectively, the honest options are ranked, and one of them is
much cheaper than I expected.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Masking** | The model absorbs the anomaly into its parameters, so the anomaly no longer looks anomalous. |
| **Stouffer's method** | Add up $z$-scores across channels; best when *everything* is affected. |
| **PSIS-LOO / Pareto $\hat k$** | A cheap way to approximate "refit without this observation" — with a built-in warning flag $\hat k > 0.7$ when it's unreliable. |
| **Woodbury / rank-one downdate** | A linear-algebra shortcut for undoing one observation's contribution to a fitted curvature matrix. |
| **Infinitesimal jackknife** | One Newton step away from the full fit, approximating a leave-one-out refit. |
| **EM with Laplace inner steps** | Practical algorithm for fitting a model with latent on/off indicators. |

### The Mathematical Mechanics

**Across channels.** With exactly-uniform per-channel $p$-values $p_{r,s}$ from the
§5 layers, sorted as $p_{(1)} \le \dots \le p_{(S)}$:

$$\mathrm{HC}_r \;=\; \max_{1 \le i \le S/2} \; \sqrt{S}\; \frac{i/S - p_{(i)}}{\sqrt{p_{(i)}\bigl(1 - p_{(i)}\bigr)}}$$

- system-wide stall → the **sum** (Stouffer) is rate-optimal;
- single-site stall → the **max**;
- unknown → **HC**, which attains the whole boundary adaptively.

**Does sharing $g_D$ across channels help?** Both ways, and the answer is pleasingly
concrete:

| | effect of sharing $g_D$ |
|---|---|
| **Prospective** (clean posterior) | Helps unambiguously — the nuisance is estimated from $S \times T$ observations, so scores are cleaner. |
| **Retrospective, one channel stalls** | **Helps.** The contamination of $\hat g_D$ is diluted to $O(1/S)$ — sharing *quarantines* it. |
| **Retrospective, all channels stall** | **Hurts.** The tail of $\hat g_D$ is biased with weight $O(1)$ and **every** channel is masked at once. |

Fable's rule: fit $g_D$ shared, but when computing deletion diagnostics, **delete the
whole time-slice across all channels at once**.

**Leave-one-diagonal-out is cheap.** This was the pleasant surprise. In the Poisson
model the log-likelihood is **separable across diagonals**, $\ell(\theta) = \sum_r
\ell_r(\theta)$, hence $H = \sum_r H_r$. So deleting diagonal $r$ is one Newton step:

$$\hat\theta_{-r} \;\approx\; \hat\theta \;-\; \bigl(H - H_r\bigr)^{-1}\,\nabla \ell_r(\hat\theta), \qquad \Pi_{-r} \;\approx\; \mathcal{N}\bigl(\hat\theta_{-r},\; (H - H_r)^{-1}\bigr)$$

And $\ell_r$ touches $\theta$ only through a handful of local values (the few $f(t)$
near the diagonal, plus the $g_D$ parameters), so $H_r$ is **low rank** and
$(H - H_r)^{-1}$ is a Woodbury downdate. **Cost: one small solve per diagonal.** We
already store the Laplace mode and precision, so this is directly implementable.

**Importance sampling is not the shortcut.** The natural trick — reweight posterior
draws by $w \propto 1/p(y_r \mid \theta)$ — has finite weight variance only if
$\int \pi(\theta \mid y)/p(y_r\mid\theta)\,d\theta < \infty$. For an anomalous
diagonal, $p(y_r \mid \theta)$ is tiny and steeply varying, so the weights explode
**exactly at the diagonals we care about**. The Pareto $\hat k$ diagnostic will exceed
$0.7$ precisely at batch candidates. Use $\hat k$ as a **flag**, then do the Newton
downdate for the flagged diagonals only.

**And do not "robustly" trim.** Fable is explicit: trimming discards the totals, which
Theorem 10 says are the *cleanest* thing we have — and the trimming rule is itself an
uncalibrated detector.

### The Bottom Line

Build the **prospective** detector first: it's honest, it's simple, and its $p$-values
need no repair. For retrospective use, the leave-one-diagonal-out downdate is cheap
and directly compatible with our existing Laplace machinery. The principled endpoint —
a sparse $\psi_r$ with a spike-and-slab prior, giving $\Pr(r \text{ is a batch} \mid
y)$ directly — is exactly the identifiability result of §9 turned into an algorithm.

---

## 12. Where it breaks, honestly

### The Core Intuition

Every method has a regime where it dies. Knowing *which* method dies *where* is what
lets you combine them.

The one genuinely fatal case is worth stating plainly:

> **A hold that never releases before "today" is indistinguishable from items being
> deleted.**

If the desk is still shut at $\tau$, you see the lull and never the dump. Transport
plus truncation *equals* deletion, within the observed triangle. No statistic can fix
this; only data from after $\tau$ can. The right response is not to be cleverer — it
is to **report "hold in progress"** and stop waiting for conservation.

### Jargon Decoder

| Term | What it practically means here |
|---|---|
| **Identification failure** | Two different truths produce literally the same data. Not a power problem — an impossibility. |
| **Graceful degradation** | The detector gets weaker rather than wrong. |
| **Least favourable alternative (open)** | We haven't proved which version of the alternative is hardest, so optimality claims are provisional. |

### The Mathematical Mechanics

| situation | what happens | what to do |
|---|---|---|
| **Hold length $L$ $\gtrsim$ width of $g_D$'s support** | The lag profile at $b$ degenerates into the *origin* profile: shape tests lose their direction. But volume and deficit are at **maximum** SNR. | Lean on $(\Delta, W)$; ignore the shape layer. Degradation is in opposite directions, so the pair still works. |
| **Hold still open at $\tau$** | $\Delta_r(k) < 0$, no spike ever arrives. Equivalent to bulk deletion. | Run the one-sided deficit CUSUM; report "hold in progress". **Honest identification failure.** |
| **Heavy-tailed $g_D$** | The nonparametric tail estimate is noisy; per-item PITs concentrate. | Prefer the totals layer; be wary of shape/PIT tests. |
| **Hold straddles the window edge** | Mass enters or leaves the box; Theorem 10's exactness leaks. | Scan over $k$; the leakage is one-sided and diagnosable. |

**Still genuinely open** (Fable's own ranked list):

1. Strict optimality over the *monotone* transport class — the window-total test is exactly valid, but whether monotonicity buys extra power is unknown.
2. A central limit theorem for the localised risk-set scan (B1). The permutation calibration makes this cosmetic, not a validity gap.
3. The overlap constant $c$ in the false-alarm formula of §10. Simulate meanwhile.
4. The exact FIFO likelihood — only needed if we ever want to *repair* the array rather than merely detect.
5. Whether the exponential tilt is truly the least favourable smearing for the shape layer.

**Signed data.** Our count-cumulative (Skellam) model has increments that can be
*negative*. A batch there could be a bulk **withdrawal**, which violates the "lag only
increases" clause of §3. The definition needs weakening to *"transport of the signed
measure conserving the origin marginal"* — and Theorem 10 does survive this
generalisation.

### The Bottom Line

The conservation law is robust; the shape tests are fragile; and there is exactly one
scenario (an open hold at the horizon) that is provably hopeless. **A good
implementation therefore leads with totals, uses shape as corroboration, and reports
"hold in progress" rather than pretending.**

---

## Appendix: suggested build order

Everything above collapses into a staged plan. Stages 1–3 are small and already give a
rigorous tool.

| Stage | What to build | Needs | Size |
|---|---|---|---|
| 1 | `batch_screen()`: running median $\hat\mu$, report the $(\Delta, W)$ quadrant | nothing | S |
| 2 | Exact Poisson / NB $p$-value for $\Delta$ (§4) | the covariance formula | S |
| 3 | Calendar phase adjustment via per-phase medians (§8) | — | S |
| 4 | Model-free shape test: block-permutation Wilcoxon (§7, B2) | — | M |
| 5 | Higher criticism on PITs; **stop relying on `surprise()` for batches** (§6) | fitted $G_D$ | M |
| 6 | `batch_detect()`: three-layer conditional test on a frozen posterior (§5) | `nowcast()` | M |
| 7 | Sequential: deficit CUSUM (early warning) + matched filter (confirm) (§10) | Stage 6 | M |
| 8 | Higher criticism across strata (§11) | Stage 6 | S |
| 9 | Leave-one-diagonal-out Woodbury downdate for masking (§11) | Laplace mode + precision | L |
| 10 | Sparse $\psi_r$ spike-and-slab contamination model (§9, §11) | Stage 9 | L |

**Three things worth remembering when you come back to this document cold:**

1. **A batch moves items; it does not create them.** Everything is a test of that.
2. **$\Delta$ is just the window total minus its mean**, and it is an *exact* Poisson
   pivot under any batch whatsoever.
3. **Sparsity — not mass conservation — is what makes a batch a well-defined thing to
   estimate.**

The five exact-law claims are already checked numerically in
`devel/validate_batch_detection.R`.
