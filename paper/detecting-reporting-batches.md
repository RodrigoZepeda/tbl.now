# SOME TITLE---

### Abstract

Consider a stream of records in which each item carries two integer timestamps: an
**origin time** at which it is generated and a later **report time** at which it
enters the record, separated by a non-negative **delay**. The reporting mechanism is
not always well behaved: it can stall and then release its backlog on a single time
step, a phenomenon we call a **reporting batch**. A batch inflates a report time's
volume, its delays, and the number of distinct origin times it draws from, and it is
easily mistaken for a genuine rise in the underlying generation rate. We formalise a
batch as an *adapted, mass-conserving transport* on the report axis. The definition
delivers one genuinely exact object and one feasible test built on it. The exact
object is a moving window of report times: because transport merely relabels the
report time of items that already exist, the *window total* is pathwise invariant to
any transport confined to the window, so its null law is exactly the law it would
have had with no batch — a finite-sample, distribution-free conservation law that
holds for every generation-rate profile and delay distribution. Given the window
total, the allocation of reports across the window is multinomial and a batch loads
the release step, giving a one-sided binomial tail; this allocation test is **exactly
similar only when the null means are known**, because its success probability
$\pi_r = \mu_r/M_r(k)$ still depends on the unknown rate and delay through $\mu_r$.
Conditioning on the window total reduces the test's sensitivity to the overall level
but does not by itself remove this nuisance: the level in the allocation
probabilities is supplied by an external, model-free baseline. We estimate the null
mean with a leave-window-out repeated-median line, give the window-width rule that
keeps the estimator uncontaminated by the episode it must measure, and show that
substituting the estimate for the true mean makes the feasible test approximate with
a size distortion that is second order in the estimation error. We extend the
reference law to overdispersed (negative-binomial) counts, prove identifiability and
robust estimation of a periodic reporting schedule, and control the false discovery
rate across all report times by the Benjamini–Hochberg procedure.

**Keywords:** reporting delay; two-timescale counting process; similar test; Neyman
structure; conservation law; repeated median; false discovery rate.

---

## 1. Introduction

Consider a stream of records with the following two-clock structure. Each item $i$
carries an **origin time** $t_i$, at which it is generated, and a later **report
time** $r_i \ge t_i$, at which it enters the record; the gap $d_i = r_i - t_i \ge 0$
is its **delay**. An observer at a horizon $\tau$ sees only the items with
$r_i \le \tau$. The natural generative description indexes items by their origin: each
origin time $t$ produces a random number of items, and each item independently waits
a random delay drawn from a fixed delay distribution. This is a model on the
*origin* axis, and it is smooth there by construction.

Such a model has no vocabulary for an event on the *report* axis. Yet report-axis
events are common: the recording mechanism stalls for several steps and then flushes
its backlog in one step; a queue is cleared all at once; a nightly job fails and
doubles up the next step. We call any such episode a **reporting batch**. A batch is
purely a rearrangement of *when* existing items are recorded — it creates and
destroys nothing — but on the report axis it produces a lull followed by a spike that
is easily confused with a genuine increase in the generation rate.

The two must be told apart. A batch is a *transport* of items along the report axis
that conserves total mass; a genuine increase is a *creation* that adds mass. Our
contribution is to define the batch so that its defining property — conservation of
mass under transport — becomes a *statistical* pivot. Conservation gives a window
total whose null law is exact in finite samples and free of every nuisance parameter
(Theorem 1); the allocation test built on it is exactly similar when the null means
are known and, in the feasible form that estimates them from the data, approximately
so, with a size distortion of second order. Either way the test fits no model for the
generation rate or the delay distribution first, which makes it usable as a screen
*before* any such model is chosen.

The idea that a total over a suitable window is invariant to a within-window
rearrangement is elementary; its value here is that it converts a vague "look for
anomalous report times" into a conditional test in the style of Neyman structure
(Lehmann and Romano, 2005, ch. 4–5), conditioning on the window total to damp
sensitivity to the overall level. We combine it with a robust, model-free baseline
(Siegel, 1982) that supplies the level in the allocation probabilities, an
overdispersion correction, an identifiability result for periodic reporting
schedules, and false-discovery-rate control (Benjamini and Hochberg, 1995) to obtain
a complete procedure.

## 2. Notation: two clocks and the observed triangle

Time is discrete and indexed by integers. Each item $i$ has an **origin time** $t_i$
and a **report time** $r_i$, with $r_i \ge t_i$; its **delay** is
$d_i = r_i - t_i \ge 0$. Since $r = t + d$, only two of the three coordinates are
free, and the data live on the lattice $\{(t,d) : t \in \mathbb Z,\ d \ge 0\}$.

Let

$$
n_{t,d} \;=\; \#\{\, i : t_i = t,\ d_i = d \,\}
$$

be the number of items with origin $t$ and delay $d$. Observation is at a horizon
$\tau$ ("now"): an item is visible iff $r_i = t_i + d_i \le \tau$, so the observed
region is the **triangle** $\{(t,d) : t + d \le \tau\}$.

A fixed **report time** $r$ is the anti-diagonal $\{(t,d) : t + d = r\}$. Two
diagonal summaries organise everything below. The **diagonal total** (reports that
arrived at time $r$),

$$
R_r \;=\; \sum_{t \le r} n_{t,\,r-t},
$$

and the **diagonal profile** (which delays those reports had),

$$
c_r(\delta) \;=\; n_{r-\delta,\,\delta}, \qquad \delta = 0,1,\dots, \qquad
R_r = \sum_{\delta} c_r(\delta).
$$

Two elementary facts about the geometry are used repeatedly. First, the triangle
**never cuts a diagonal in half**: for $r \le \tau$ the entire anti-diagonal is
observed, so diagonal totals need no truncation correction. Second, **on a single
diagonal the delay is a deterministic function of the origin** ($\delta = r - t$), so
there is no origin–delay association to test *within* a diagonal; associations must
be assessed *across* neighbouring report times.

## 3. The generative null model

Write $\lambda_t \ge 0$ for a smooth generation intensity on the origin axis and
$g_D$ for a delay probability mass function on $\{0,1,2,\dots\}$. The **clean model**
is

$$
N_t \sim \operatorname{Poisson}(\lambda_t) \text{ independently}, \qquad
\text{each item's delay} \sim g_D \text{ i.i.d.}, \qquad
\log\lambda_t \text{ smooth}.
\tag{M}
$$

We first develop the theory under (M); §7 introduces a per-origin gamma frailty to
accommodate overdispersion, and §8 a periodic reporting multiplier.

**Lemma 1 (Poisson colouring).** *Under (M), the cell counts are independent with*
$$
n_{t,d} \sim \operatorname{Poisson}\!\big(\lambda_t\, g_D(d)\big),
\qquad \text{independent across all } (t,d).
$$

*Proof.* Colour each of the $N_t \sim \operatorname{Poisson}(\lambda_t)$ items
originating at $t$ independently by its delay, delay $d$ having probability
$g_D(d)$. By the colouring theorem for Poisson variables (Kingman, 1993, §5), the
per-colour counts $(n_{t,d})_{d\ge 0}$ are independent Poissons with means
$\lambda_t g_D(d)$. Independence across $t$ is inherited from the independence of the
$N_t$. $\qquad\blacksquare$

**Lemma 2 (diagonal totals).** *Under (M), the diagonal totals $R_r$ are independent
across $r$ with*
$$
R_r \sim \operatorname{Poisson}(\mu_r), \qquad
\mu_r \;=\; \sum_{t\le r} \lambda_t\, g_D(r-t) \;=\; (\lambda * g_D)(r).
$$

*Proof.* $R_r$ is a sum over the anti-diagonal $t + d = r$ of the independent cells
of Lemma 1; the sum of independent Poissons is Poisson with the summed mean. Distinct
diagonals are disjoint sets of cells, hence independent. $\qquad\blacksquare$

The mean is a **convolution**, and convolution with a probability mass function is a
low-pass operation: $|\widehat{g_D}(\omega)| \le 1$ for all frequencies $\omega$, with
strict inequality unless $g_D$ is degenerate, since
$1 - |\widehat{g_D}(\omega)|^2 = \sum_{d<d'} 2 g_D(d)g_D(d')\{1-\cos\omega(d-d')\} > 0$
whenever $g_D$ charges two distinct delays. Thus the *mean* report series $\mu$ is
smoother than $\lambda$: the clean model **cannot** manufacture sharp, high-frequency
structure on the report axis. A batch is exactly such structure; it lives in the
model's blind spot. We now make this exploitable.

## 4. Batches as adapted transports

Each item has an **ideal** report time it would have had under (M),
$r_i^\star = t_i + D_i$ with $D_i \sim g_D$, giving ideal diagonal totals
$R_r^\star = \#\{i : r_i^\star = r\}$, which by Lemma 2 are independent
$\operatorname{Poisson}(\mu_r)$.

**Definition 1 (transport).** A *transport* is a (possibly random) map that assigns
each item an observed report time $r_i$ such that

1. **mass conservation** — items are neither created nor destroyed;
2. **origin preservation** — $t_i$ is untouched;
3. **monotonicity** — $r_i \ge r_i^\star$ (an item can only be recorded *later*);
4. **adaptedness** — the assignment of $r_i$ may depend on the history
   $\mathcal F_{r_i^\star}$ but not on the future.

A closure schedule is the archetype: for a set $H$ of closed days, the
*next-open-day* map $\varrho(u) = \min\{v \ge u : v \notin H\}$ sends
$r_i = \varrho(r_i^\star)$, releasing every held item on the first open day. This
satisfies (1)–(4), as does first-in-first-out recovery over several days.

A **creation**, by contrast, adds items: it increases some $\lambda_t$ or injects
extra origins, and does not conserve mass.

## 5. The conservation law and exact ancillarity

Fix a candidate report time $r$ and a look-back $k \ge 1$, and let
$\mathcal W = \{r-k, r-k+1, \dots, r\}$ be the **window**. Define the **window
total** and its **null mean**

$$
S_r(k) = \sum_{j\in\mathcal W} R_j, \qquad
M_r(k) = \sum_{j\in\mathcal W} \mu_j .
$$

**Definition 2 ($\mathcal W$-closed transport).** A transport (Definition 1) is
*$\mathcal W$-closed* if, on every realisation, the set of items observed in
$\mathcal W$ coincides with the set ideally reported in $\mathcal W$,
$$
\{i : r_i \in \mathcal W\} \;=\; \{i : r_i^\star \in \mathcal W\}.
$$
Equivalently, two boundary conditions hold pathwise: **(a) no outward escape** — no
item with $r_i^\star \in \mathcal W$ is pushed to a report time past $\max\mathcal
W$; and **(b) no inward crossing of the left boundary** — no item with $r_i^\star <
\min\mathcal W$ is released *into* $\mathcal W$. A closure that holds a set $H
\subseteq \mathcal W$ of days and releases on $r$ is $\mathcal W$-closed exactly when
(a) and (b) hold; the two ways they can fail are the one-sided leaks catalogued in
§5.3. Condition (b) in particular is what Theorem 1 needs and is stated here rather
than assumed silently.

**Theorem 1 (pathwise invariance).** *Under (M) and any $\mathcal W$-closed transport
(Definition 2), the window total is pathwise invariant,*
$$
S_r(k) \;=\; \sum_{j\in\mathcal W} R_j^\star \quad \text{almost surely},
$$
*and therefore, exactly and independently of the transport,*
$$
S_r(k) \sim \operatorname{Poisson}\!\big(M_r(k)\big).
$$

*Proof.* By $\mathcal W$-closedness, $\{i : r_i \in \mathcal W\} = \{i : r_i^\star \in
\mathcal W\}$ as sets of items, on every realisation of the transport randomness.
Counting the two sides,
$S_r(k) = \#\{i : r_i \in \mathcal W\} = \#\{i : r_i^\star \in \mathcal W\} =
\sum_{j\in\mathcal W} R_j^\star$. The right-hand side is a sum of the disjoint,
independent Poisson cells on the anti-diagonals $j \in \mathcal W$ (Lemma 1), hence
$\operatorname{Poisson}(M_r(k))$. Because the identity holds for every value of the
transport's internal randomness, the conditional law of $S_r(k)$ given that
randomness is $\operatorname{Poisson}(M_r(k))$; averaging leaves it unchanged.
$\qquad\blacksquare$

Thus $S_r(k)$ is **ancillary for the transport**: its distribution is the same
whether or not a batch occurred, and however strong the batch is. A creation adding
expected mass $\eta$ to the window instead gives
$S_r(k) \sim \operatorname{Poisson}(M_r(k) + \eta)$. Writing
$\Delta_r(k) = S_r(k) - M_r(k)$, we have $\mathbb E[\Delta_r(k)] = 0$ under any batch
and $\mathbb E[\Delta_r(k)] = \eta > 0$ under a creation: $\Delta$ is the
**creation** signal, blind to transport.

### 5.1 The conditional transport test

Transport is invisible to the window *total* but not to the *allocation* of that
total across the window's days. Conditioning on the total fixes the overall level and
leaves a law governed only by the *relative* means over the window — which still
depend on the nuisance, a dependence we return to after stating the allocation law.

**Theorem 2 (conditional allocation).** *Under the clean model (no transport), given
$S_r(k) = n$,*
$$
\big(R_j\big)_{j\in\mathcal W} \;\big|\; S_r(k) = n
\;\sim\; \operatorname{Multinomial}\!\Big(n,\ \big(\mu_j / M_r(k)\big)_{j\in\mathcal W}\Big).
$$
*In particular the release-day count satisfies*
$$
R_r \;\big|\; S_r(k) = n \;\sim\; \operatorname{Binomial}\!\big(n,\ \pi_r\big),
\qquad \pi_r = \mu_r / M_r(k).
\tag{5.1}
$$

*Proof.* The $(R_j)_{j\in\mathcal W}$ are independent
$\operatorname{Poisson}(\mu_j)$ (Lemma 2); independent Poissons conditioned on their
sum are multinomial with probabilities proportional to the means (a standard
property). Marginalising the multinomial to the single coordinate $j = r$ gives the
binomial. $\qquad\blacksquare$

A $\mathcal W$-closed batch releases held items on day $r$, inflating $R_r$ while
holding $S_r(k)$ fixed (Theorem 1). It therefore pushes $R_r$ into the **upper tail**
of (5.1). This yields the **oracle transport test** — oracle because it treats the
success probability $\pi_r$ as known — which rejects for large $R_r$ using the
one-sided binomial tail

$$
p^{\mathrm{trans}}_r
\;=\; \Pr\!\big\{ \operatorname{Binomial}(n, \pi_r) \ge R_r \big\}
\;=\; \sum_{x \ge R_r} \binom{n}{x} \pi_r^{x}(1-\pi_r)^{\,n-x},
\qquad n = S_r(k).
\tag{5.2}
$$

**Proposition 1 (similarity of the oracle test).** *Fix the ratios
$\pi_j = \mu_j/M_r(k)$, $j\in\mathcal W$. Under the clean model, the test that
rejects when $p^{\mathrm{trans}}_r \le \alpha$ has conditional size $\le \alpha$
given $S_r(k)$, and hence unconditional size $\le \alpha$, for every $\lambda$ and
$g_D$ consistent with those ratios.*

*Proof.* Conditionally on $S_r(k) = n$, the statistic $R_r$ has the binomial law
(5.1) with success probability $\pi_r$; once $\pi_r$ is fixed, this law does not
depend on any further feature of $(\lambda, g_D)$, so the binomial tail is a valid
conditional $p$-value and the test is conditionally of level $\alpha$. A test of
conditional level $\alpha$ given $S_r(k)$ is unconditionally of level $\alpha$
(Lehmann and Romano, 2005, Thm 4.4.1). $\;\blacksquare$

The scope of Proposition 1 is exactly its limitation. The conditional law (5.1) is
free of the nuisance *only after* the allocation probabilities $\pi_j$ are fixed, and
$\pi_r = \mu_r/M_r(k)$ is itself a functional of the unknown $\lambda$ and $g_D$.
Conditioning on $S_r(k) = n$ therefore does not by itself remove the nuisance: it
removes the *overall level* $M_r(k)$ — the total number of items in play — while the
*shape* of the null mean over the window survives inside $\pi_r$. The window total is
a cut in the weaker, informal sense of Barndorff-Nielsen (1978), carrying the level
but not the within-window allocation; it is **not** a complete sufficient statistic
for the high-dimensional nuisance $(\lambda, g_D)$ (the full data are the cell counts
$n_{t,d}$, of which $S_r(k)$ is a one-dimensional summary), so no Neyman-structure
theorem forces the conditioning or delivers exact similarity on its own. What the
conditioning buys is reduced sensitivity to the level; the remaining nuisance in the
$\pi_j$ is supplied not by sufficiency but by an *external* model-free estimate of
the null mean, developed in §6.

**From oracle to feasible test.** Because $\pi_r$ is unknown, the procedure actually
run replaces $\mu_j$ by the leave-window-out baseline $\widehat\mu_j$ of §6, forming
$\widehat\pi_r = \widehat\mu_r/\widehat M_r(k)$ and the *feasible* binomial tail
$\widehat p^{\mathrm{trans}}_r$ from (5.2) with $\pi_r \mapsto \widehat\pi_r$. This is
the transition on which everything downstream depends, and it is a genuine one: the
feasible test is **approximate**, not exactly similar. Its size distortion is
governed by the estimation error $\widehat\pi_r - \pi_r$, which is $O_P(w^{-1/2})$ for
a baseline window of width $w$; because the conditioning already absorbs the
first-order effect of the level, the residual distortion is of second order (§6). The
exactness we do retain unconditionally is that of Theorem 1: the window total's null
law, and hence the transport-invariance of the discriminant $\Delta$, needs no
estimated mean at all.

### 5.2 The deficit form, and a heuristic for the unweighted contrast

An equivalent contrast is the **deficit** accumulated before $r$,

$$
W_r(k) \;=\; \sum_{j=r-k}^{r-1} \big(\mu_j - R_j\big),
$$

so that the identity
$\Delta_r(k) = (R_r - \mu_r) - W_r(k) = S_r(k) - M_r(k)$ holds. The first fact about
$\Delta$ is exact and needs no efficiency theory. Among all linear contrasts
$a\,(\text{excess at }r) + b\,(\text{deficit before }r)$, demanding that the
expectation equal the creation mass $\eta$ for *every* hold magnitude $m$ — i.e.
$a(\eta+m) + b\,m = \eta$ for all $m$ — forces $a=1,\ b=-1$. So $\Delta = S_r(k) -
M_r(k)$ is the *unique* linear combination whose mean is blind to transport, which is
why it is the creation discriminant.

The choice of the *unweighted* deficit for the transport direction is a different
matter, and we present it as a **heuristic motivation** rather than a theorem; a full
minimax statement would require a decision-theoretic setup (loss, action space,
least-favourable prior) we do not develop here. The heuristic is a local score
calculation under the Poisson model (M). Parametrise a departure by a creation
strength $\eta$ (extra mass added at $r$) and a hold that thins the pre-release days
by weights $w_j \ge 0$, $j<r$, with $\sum_{j<r} w_j = 1$ fixing the hold's shape and a
magnitude $m$ fixing its size. The efficient scores at the null $(\eta,m)=(0,0)$,
using $\partial \log\Pr\{R_j = \cdot\}/\partial(\cdot)$ for the Poisson pmf, are

$$
U_\eta = \frac{R_r}{\mu_r} - 1
\quad\text{(score for creation strength $\eta$)}, \qquad
U_m = U_\eta + \sum_{j<r} w_j\Big(1 - \frac{R_j}{\mu_j}\Big)
\quad\text{(score for hold magnitude $m$),}
$$

each having null mean zero and variance $\sum_j w_j^2/\mu_j$ for the hold part. The
component of the transport score $U_m$ orthogonal to the creation score $U_\eta$ is
the weighted relative deficit $\sum_{j<r} w_j(1 - R_j/\mu_j)$. Treating the hold shape
$w$ as adversarial, the *least-favourable* (hardest to detect) shape maximises the
score variance $\sum_j w_j^2/\mu_j$ subject to $\sum_j w_j = 1$, giving proportional
thinning $w_j \propto \mu_j$; against that shape the locally most powerful statistic
is the *unweighted* deficit $W_r(k) = \sum_{j<r}(\mu_j - R_j)$. This motivates
standardising the unweighted deficit directly in the robust regime (§7); we do not
claim it as a proven minimax rule. Readers wanting the formal machinery for
score-based least-favourable directions may consult Lehmann and Romano (2005, ch. 12–
13).

### 5.3 Where exactness leaks — and how the scan detects it

Exactness of Theorem 1 requires $\mathcal W$-closedness. Two departures break it, and
both are one-sided and diagnosable by varying $k$:

* **Edge straddle.** If the hold begins before $\min\mathcal W$ and releases inside
  it, mass *enters* the window and $S_r(k)$ is inflated — mimicking a creation.
  Widening $k$ until $\Delta$ stabilises separates this from true creation.
* **Truncation ($r$-release past $\tau$).** If held items would be released after the
  horizon, they are simply absent: transport plus right-truncation equals *deletion*,
  giving $\Delta_r(k) < 0$. A significant negative $\Delta$ (equivalently a
  lower-tail Poisson $p$-value) flags an *ongoing* hold rather than a completed
  batch, which we report separately and never as a batch.

## 6. From the oracle to the feasible test: the baseline

Everything up to here is the **oracle** construction: the tail (5.2) and the deficit
are written in terms of the true null mean $\mu_r$, which under (M) is the unknown
convolution $(\lambda * g_D)(r)$. This section makes the test **feasible** by
replacing $\mu_r$ with a robust local estimate $\widehat\mu_r$ computed from the
report totals alone, and it is the step that turns the exactly similar oracle test of
§5.1 into an approximate one. We flag the transition explicitly because it is easy to
read (5.2) as computable without any nuisance estimation — it is not; the binomial
$p$-value depends on $\pi_r = \mu_r/M_r(k)$, and $\mu_r$ must be supplied. Two
requirements shape the estimator: it must (i) be robust to the batch episode itself,
which corrupts a run of consecutive report times, and (ii) not be biased by the
smooth trend in $\lambda$.

We use **Siegel's repeated median** (Siegel, 1982), a local-linear smoother with the
maximal breakdown point $1/2$, fitted **directly to the raw report totals $R_r$**
against report position (not to $\log R_r$). On a window of report positions it fits
a line by taking, for each point, the median of the pairwise slopes through it, then
the median of those medians for the slope, and the median of the residuals for the
intercept.

**Lemma 3 (window-width rule).** *A batch episode with look-back $L$ corrupts at most
$L+1$ consecutive report times ($L$ deficit steps and one release). A moving repeated
median of odd width $w$, fitted to the raw totals $R_r$, is uncontaminated by the
episode at every clean centre — its breakdown is not exceeded — provided*
$$
w \;\ge\; 2L + 3,
$$
*and, on report times over which the mean $\mu_r$ is exactly linear in $r$, it is
unbiased at those centres. When $\mu_r$ is smooth but curved, the residual bias is
the curvature term $O(w^2 \sup|\mu''|)$ over the fitting window, which is $o(1)$ as
the window narrows.*

*Proof.* The repeated median has breakdown point $\lfloor (w+1)/2\rfloor / w \to 1/2$
(Siegel, 1982): the fit is unchanged by altering strictly fewer than half the points
in the window. The episode contributes at most $L+1$ contaminated points; for a
centred window of width $w$ the number of contaminated points seen is at most $L+1$,
and $w \ge 2L+3$ gives $L+1 < w/2$, so fewer than half are corrupt and the fitted
line equals the fit to the clean majority. If the clean majority has means $\mathbb
E[R_j] = \mu_j$ lying exactly on a line in $j$, the repeated-median line reproduces
that line exactly (the repeated median is exact for data on a line and unbiased for
the additive noise around it), so the fitted value at the centre is unbiased for
$\mu$ there. For a twice-differentiable $\mu$, a Taylor expansion over the width-$w$
window leaves the stated curvature remainder. $\qquad\blacksquare$

We fit on the **raw** scale, so the relevant local-linearity condition is on $\mu$
itself, not on $\log\mu$. The two agree to first order over a short window — where
$\mu$ is smooth, $\log\mu$ smooth is equivalent — and the low-pass argument of §3
(that $\mu$ is a smoothed version of $\lambda$) is what makes either approximation
good over the narrow baseline windows used in practice. By contrast a running *mean*
of width $w$ carries bias $O(\text{batch mass}/w)$ across a whole $2w$-neighbourhood —
enough to let the deficit drag the baseline down so the batch masks itself as a
creation; a running *median* (local constant) is robust but biased wherever $\mu$
trends, calling every rising time a creation. The repeated-median line has neither
defect, which is why we adopt it as the sole baseline.

**Leave-window-out refitting.** For a *candidate* window $\mathcal W$ the estimate
$\widehat\mu$ used inside the test is refit from the nearest report times lying
strictly *outside* $\mathcal W$ and extrapolated across it. Because the transport is
$\mathcal W$-closed, dates outside $\mathcal W$ cannot contain the episode, so
$\widehat\mu_{\mathcal W}$ is uncontaminated by construction, and the batch cannot
inflate the very baseline it is measured against. This step is described once here;
the algorithm of §10 refers back to it rather than restating it. Two remarks. First,
the estimation error in $\widehat\mu$ enters the statistics additively and is
$O_P(w^{-1/2})$; because the conditioning of §5.1 removes the first-order effect of
the level, the residual size distortion of the feasible test is second order in that
error. Second, a separate whole-series repeated median $\widehat\mu^{\,\mathrm{glob}}$,
defined everywhere, is used only for the two quantities that tolerate mild
contamination and need a global summary: the dispersion estimate (§7) and the
periodic phase factors (§8). It is never used for $\Delta$.

## 7. Overdispersion: the negative-binomial null

Real counts are often over-dispersed relative to Poisson, from a per-origin random
effect (some origin times are busier than the smooth trend predicts). Model it as a
gamma frailty: $\Lambda_t = \lambda_t\, G_t$ with
$G_t \sim \operatorname{Gamma}(\rho, \rho)$ (mean $1$, variance $1/\rho$),
independent across $t$. Then $N_t$ is negative binomial and, by colouring, the cells
$n_{t,d}$ inherit a shared per-origin frailty.

**Lemma 4 (diagonal covariance under frailty).** *Under the gamma-frailty model,*
$$
\operatorname{Cov}(R_r, R_{r'})
= \mathbf 1\{r=r'\} \sum_{t\le r} m_t(r)
\;+\; \frac1\rho \sum_{t \le r\wedge r'} m_t(r)\,m_t(r'),
\qquad m_t(r) = \lambda_t g_D(r-t).
$$

*Proof.* Condition on the frailties $G$: the cells are independent Poisson with means
$G_t m_t(\cdot)$, giving the diagonal (Poisson) term $\mathbf 1\{r=r'\}\sum_t
G_t m_t(r)$ and, since $R_r, R_{r'}$ are conditionally independent, no cross term.
Taking expectations over $G$ returns the Poisson part with $\mathbb E G_t = 1$. The
covariance of the conditional means, $\operatorname{Cov}(\sum_t G_t m_t(r), \sum_{t'}
G_{t'} m_{t'}(r'))$, is $\sum_t \operatorname{Var}(G_t) m_t(r) m_t(r') =
\rho^{-1}\sum_{t\le r\wedge r'} m_t(r) m_t(r')$ by independence of the $G_t$. Summing
the two contributions gives the stated covariance. $\qquad\blacksquare$

Two consequences. First, the marginal variance is
$\operatorname{Var}(R_r) = \mu_r(1 + \mu_r/\rho\cdot\bar m_r)$, quasi-Poisson of the
form $\phi\,\mu_r$ to first order, motivating a single dispersion multiplier $\phi$.
Second, the cross-covariance runs *only* through origins feeding both diagonals, so
$\operatorname{Cov}(R_r, R_{r'}) = 0$ once $|r-r'|$ exceeds the support width of
$g_D$: diagonals separated by more than the delay range are uncorrelated. This local
dependence is what block-based resampling (for the companion shape test) and the FDR
argument (§9) must respect.

**Robust reference law.** With over-dispersion the window total is no longer exactly
Poisson, but Theorem 1's ancillarity (pathwise mass conservation) survives — only the
reference law changes. We standardise the two contrasts by a robust scale and read
normal tails:

$$
z^{\Delta}_r = \frac{\Delta_r(k)}{\sqrt{\phi\, V^{\mathrm{win}}_r}}, \qquad
z^{W}_r = \frac{W_r(k)}{\sqrt{\phi\, V^{\mathrm{def}}_r}},
$$

with $V^{\mathrm{win}}_r = \sum_{j\in\mathcal W}\max(|\widehat\mu_j|,1)$,
$V^{\mathrm{def}}_r = \sum_{j<r}\max(|\widehat\mu_j|,1)$, and
$p^{\mathrm{trans}}_r = 1 - \Phi(z^{W}_r)$, $p^{\mathrm{creat}}_r = 1 - \Phi(z^\Delta_r)$.

The two variance terms deserve a word of justification, since neither the floor nor
the $\max$ is arbitrary. The contrasts $\Delta_r(k)$ and $W_r(k)$ are signed sums of
approximately independent counts over the window (Lemma 4 makes the dependence
short-range, so within a window the terms are nearly uncorrelated). Under a
quasi-Poisson variance function, $\operatorname{Var}(R_j) \approx \phi\,\mu_j$, the
variance of such a sum is $\phi\sum_j \mu_j$, which is what $\phi\,V^{\mathrm{win}}_r$
estimates once $\mu_j$ is replaced by the baseline $\widehat\mu_j$: the sum of
baseline levels over the window *is* the natural variance normaliser for a
Poisson-type contrast. The $\max(|\widehat\mu_j|,1)$ is a **variance floor**: on
low-count or count-cumulative series a fitted $\widehat\mu_j$ can be near zero or —
for signed increments — negative, and dividing by $\sqrt{\phi\sum_j\widehat\mu_j}$
would then be division by a vanishing or ill-defined quantity, producing spurious
large $z$. Flooring each term at $1$ (one expected count, the smallest meaningful
Poisson variance) caps the standardisation and makes it monotone in $|\widehat\mu_j|$;
it is deliberately mildly *conservative*, inflating the denominator and shrinking $z$
where counts are tiny. The normal approximation to $z^\Delta_r$ and $z^W_r$ is then a
central limit statement for a sum of many weakly and short-range dependent counts:
with $m$-dependence of range the delay-support width (Lemma 4), a CLT for
$m$-dependent triangular arrays (e.g. Hoeffding–Robbins) gives asymptotic normality
of the standardised contrast as the window widens, and simulation (§11) confirms the
approximation is adequate at the window widths used. The dispersion is estimated
robustly from Pearson residuals of the global baseline,

$$
\widehat\phi = \operatorname{mad}\Big(\big\{ (R_r - \widehat\mu^{\,\mathrm{glob}}_r)/
\sqrt{\max(|\widehat\mu^{\,\mathrm{glob}}_r|,1)} \big\}_r\Big)^2,
$$

the median absolute deviation preventing the episode from inflating the very scale it
is judged against. This robust null is *also* the correct one for **cumulative**
report streams, where the recorded quantity is a running total and the per-step
increments are signed differences of counting processes: the window total is then a
difference rather than a count, so the exact Poisson tail no longer applies but the
standardised contrast does. The choice of reference law is automatic — the exact
Poisson/binomial null (5.2) only when the counts are non-negative and no
over-dispersion is detected ($\widehat\phi \le 1.5$), the robust null otherwise —
because the exact Poisson tail is anti-conservative on over-dispersed counts.

## 8. The periodic confounder

A reporting mechanism that is *always* inactive at one phase of a fixed cycle — say
every $P$-th step — produces, every cycle, all the batch symptoms: a lull, a spike,
inflated delays, many origins. This is a *scheduled* transport, and the relevant null
is "clean model **plus** a periodic schedule". A batch is an *irregular* excursion
relative to the schedule. Enrich the cell mean with a $P$-periodic report-index
multiplier $\psi_r$ (period $P$):

$$
\mathbb E\, n_{t,d} \;=\; \psi_{t+d}\; \lambda_t\; g_D(d), \qquad \psi_{r+P}=\psi_r.
$$

Two questions arise: is $\psi$ **identifiable** (separable from $\lambda$ and $g_D$),
and can it be estimated without the batches contaminating it?

**Theorem 3 (identifiability of the schedule).** *The periodic multiplier $\psi$ is
identified up to a global scale if and only if the delay distribution $g_D$ charges
two lags of different parity — in particular if it charges two **adjacent** lags
$\delta, \delta+1$.*

*Proof.* Seek reparameterisations leaving every cell mean fixed. Multiplying
$\psi_r \mapsto \psi_r\,\omega^{-r}$, $\lambda_t \mapsto \lambda_t\,\omega^{t}$,
$g_D(d)\mapsto g_D(d)\,\omega^{d}$ with $\omega^P = 1$ changes the $(t,d)$ cell mean by
$\omega^{-(t+d)}\omega^{t}\omega^{d} = \omega^0 = 1$; these are exactly the aliases (a
character of $\mathbb Z_P$; any invariant reparameterisation is of this form because
the log-mean is additive in the three factors and periodicity constrains the
report-index part to characters of $\mathbb Z_P$). Now $g_D$ must remain a real,
non-negative pmf. A non-real $\omega$ makes $g_D(d)\,\omega^{d}$ non-real for any $d$
in the support, and is excluded. The only real $P$-th roots of unity are $\omega=1$
(the trivial alias) and, when $P$ is even, $\omega=-1$, which sends
$g_D(d)\mapsto (-1)^d g_D(d)$; this stays non-negative only if the support of $g_D$
lies entirely in one parity class (all-even or all-odd $d$). Hence if $g_D$ charges
two lags of different parity — a fortiori two adjacent lags — no non-trivial alias
survives and $\psi$ is identified up to the global scale $\omega=1$ fixes by
normalisation. Conversely, if the support is confined to one parity class, $\omega=-1$
is a genuine alias and $\psi$ is not identified. $\qquad\blacksquare$

The condition is mild: any realistic delay distribution charges adjacent lags, so a
periodic reporting cycle is identified whatever the period.

**Robust phase estimation.** Identifiability does not stop batches from contaminating
an estimate of $\psi$. Estimate each phase multiplier by the **median across cycles**
of the observed-to-baseline ratio at that phase, renormalised to geometric mean one:

$$
\widehat\psi_{\,p} \;=\; \operatorname{median}\Big\{ R_r/\widehat\mu^{\,\mathrm{glob}}_r
\;:\; r \equiv p \ (\mathrm{mod}\ P)\Big\}, \qquad p = 0,\dots,P-1,
$$

keeping structural zeros (a phase on which the mechanism is always inactive) so a
closed phase is not mistaken for a lull.

**Proposition 2 (robust recovery of the schedule).** *If, at each phase, strictly
fewer than half of the cycles are contaminated by irregular batches, then
$\widehat\psi_p$ is consistent for $\psi_p$ (up to the global scale).*

*Proof.* Within a phase the ratios $R_r/\widehat\mu^{\,\mathrm{glob}}_r$ concentrate
around $\psi_p$ under the null; batches move a strict minority of them. The median is
unchanged by altering fewer than half the values (breakdown $1/2$), so it converges to
$\psi_p$. $\qquad\blacksquare$

The window statistics of §5–7 are then computed on the phase-adjusted series
$R_r/\widehat\psi_{\,r\bmod P}$, so an *irregular* batch reads as an excursion
relative to the schedule. This remains valid even when a hold spans a whole period
($L \ge P$): locally it mimics a phase shift, but across many cycles it still
contaminates each phase in fewer than half of them.

## 9. Multiplicity: false-discovery-rate control

The scan produces a transport $p$-value $p^{\mathrm{trans}}_r$ at every report time
$r$ (and stratum). Thresholding each at $\alpha$ would flag a fraction $\approx\alpha$
of *all* dates even under the global null. We instead control the **false discovery
rate** across the full family by the Benjamini–Hochberg (BH) procedure (Benjamini and
Hochberg, 1995): order the $p$-values $p_{(1)} \le \dots \le p_{(N)}$, find the largest
$i$ with $p_{(i)} \le (i/N)\alpha$, and reject the corresponding $i$ hypotheses. The
batch verdict is precisely this BH decision (excluding times whose significance is a
*deletion* — a negative $\Delta$, an ongoing hold — since no spike has arrived
there).

**Proposition 3 (FDR control).** *If the transport $p$-values are independent, or
positively regression dependent on the subset of true nulls (PRDS), the BH procedure
controls the false discovery rate at level $\alpha\, N_0/N \le \alpha$, where $N_0$ is
the number of true nulls.*

*Proof.* This is the Benjamini–Hochberg (1995) theorem under independence and its
extension to PRDS families (Benjamini and Yekutieli, 2001, Thm 1.2). $\;\blacksquare$

Two remarks on the dependence. First, by Lemma 4 the diagonal totals are uncorrelated
beyond the delay-support width, and the window statistics of neighbouring dates
overlap only over $O(k)$ dates, so the dependence is short-range. Establishing PRDS
for the exact binomial $p$-values under this local structure is not immediate; where
a guarantee under arbitrary dependence is required, the Benjamini–Yekutieli (2001)
procedure (scaling $\alpha$ by $\sum_{i=1}^N 1/i$) controls the FDR unconditionally at
the cost of power. Second, the exact binomial null of (5.2) is conservative on
discrete supports; randomised or mid-$p$ variants restore exactness if the loss of
power matters. The default reports the conservative BH-on-exact-binomial flag, which
we regard as the trustworthy verdict.

## 10. The procedure

Given report totals $R_r$ per report time (and, if there are several parallel
streams, per stratum), a look-back $k$, a baseline width $w \ge 2k+3$, and a level
$\alpha$:

1. **Reduce** to one signed count per (origin, report) pair within each stratum;
   difference any cumulative stream into signed increments; place the totals on the
   complete report grid, filling absent report times with zero (an observed "nothing
   reported").
2. **Resolve the period.** If a period $P$ is not supplied, take it from a known
   periodicity of the index (for instance $P=7$ or $P=52$ for a short or long cycle);
   absent any indication, default to $P=7$.
3. **Baseline.** Fit the whole-series repeated-median line
   $\widehat\mu^{\,\mathrm{glob}}$; if $P$ is set, estimate phase factors
   $\widehat\psi$ by median-across-cycles (Proposition 2) and remove the periodic
   component.
4. **Window statistics.** For each $r$, form the leave-window-out baseline over
   $\mathcal W = \{r-k,\dots,r\}$ as in §6, and compute $S_r(k)$, $M_r(k)$,
   $\Delta_r(k)$, $W_r(k)$, and their variance normalisers.
5. **Reference law.** Estimate dispersion $\widehat\phi$; if the counts are
   non-negative and $\widehat\phi \le 1.5$, use the exact Poisson/binomial tails
   (5.2), else the robust normal tails of §7.
6. **Flag.** BH-adjust the transport $p$-values across all (time, stratum) pairs and
   declare a batch where the adjusted value is below $\alpha$ and $\Delta_r(k)$ is not
   significantly negative.

The output is one record per (report time, stratum) carrying the report total $R_r$,
the baseline $\widehat\mu_r$, the deficit $W_r(k)$, the discriminant $\Delta_r(k)$,
the raw and BH-adjusted transport $p$-values, and the batch verdict. The creation
signal $\Delta_r(k)$ is returned as a magnitude but *not* thresholded into a verdict:
a creation test compares only the window total against the baseline, so on a steeply
curved mean it fires on ordinary growth; separating genuine creation from curvature
requires a fitted model for the generation rate and is out of scope for a model-free
screen.

## 11. Numerical illustration

We summarise the operating characteristics on simulated streams. Under the global
null (clean model with a periodic schedule, over-dispersion $\phi \in \{1, 3, 6\}$,
$N = 250$ report times), the BH batch verdict holds the realised false discovery rate
at or below the nominal $\alpha = 0.05$ across the dispersion range, whereas
thresholding the raw per-time $p$-values yields a proportion of flagged times near
$\alpha$ *per time* — dozens of false alarms on a long series — confirming the need
for §9. Against a planted closure of length $L \le k$ released on a single step, the
feasible Poisson/binomial test (5.2 with the estimated $\widehat\pi_r$) attains power
increasing with the deficit, and its realised size stays close to nominal —
consistent with the second-order distortion argued in §6; the robust test loses
little for $\phi$ up to $6$. In a stress test where a genuine creation coincides with
a smoothly rising mean, the procedure declines to call the growth a batch, because
the window total there genuinely rises rather than merely reallocating — the
separation of transport from creation that the discriminant $\Delta$ is designed to
provide.

## 12. Discussion

The construction turns a modelling *deficiency* — the clean model cannot express a
report-axis event — into a workable test, by defining the target (a batch) as a
transport and exploiting the conservation of mass a transport implies. What is exact
and finite-sample is the conservation law itself (Theorem 1): the window total's null
law is transport-invariant, so the discriminant $\Delta$ separates transport from
creation without any fitted model. The allocation test built on it is exactly similar
only in the oracle case where the null means are known; conditioning on the window
total damps sensitivity to the overall level, but the level in the allocation
probabilities is supplied by an external robust baseline, so the feasible test is
*approximate*, with a size distortion that is second order in the baseline's
estimation error. Robust baseline estimation, an over-dispersion correction, an
identifiability result for periodic schedules, and FDR control complete a procedure
that is cheap enough to be the first thing one runs on a new report stream.

Several extensions are natural. A *shape* layer — testing whether a report time drew
from unusually old origins — is a companion rank test that is exactly
distribution-free whenever $\log\lambda$ is locally linear (so that the conditional
delay profile is common across neighbouring diagonals); in the dense–weak regime a
batch induces (many mildly inflated delays) it is far more powerful than flagging the
single most-delayed report, and a higher-criticism variant (Donoho and Jin, 2004)
adapts across the sparse–dense spectrum. A localised quasi-independence scan (Tsai,
1990; Efron and Petrosian, 1999) probes the full joint structure at higher cost.
Finally, the exact discreteness of (5.2) invites a mid-$p$ or randomised refinement,
and the local dependence of the scan statistics invites a formal PRDS analysis to
sharpen Proposition 3.

The main limitation is inherent to a model-free screen: it detects *departures of a
kind the clean model cannot produce*, and it separates transport from creation, but
it does not by itself reconstruct the true origin counts. It is a diagnostic to be
run before — not instead of — any model that does.

---

## References

Barndorff-Nielsen, O. E. (1978). *Information and Exponential Families in Statistical
Theory.* Wiley.

Benjamini, Y. and Hochberg, Y. (1995). Controlling the false discovery rate: a
practical and powerful approach to multiple testing. *Journal of the Royal
Statistical Society, Series B* **57**, 289–300.

Benjamini, Y. and Yekutieli, D. (2001). The control of the false discovery rate in
multiple testing under dependency. *Annals of Statistics* **29**, 1165–1188.

Donoho, D. and Jin, J. (2004). Higher criticism for detecting sparse heterogeneous
mixtures. *Annals of Statistics* **32**, 962–994.

Efron, B. and Petrosian, R. (1999). Nonparametric methods for doubly truncated data.
*Journal of the American Statistical Association* **94**, 824–834.

Kingman, J. F. C. (1993). *Poisson Processes.* Oxford University Press.

Lehmann, E. L. and Romano, J. P. (2005). *Testing Statistical Hypotheses*, 3rd ed.
Springer.

Siegel, A. F. (1982). Robust regression using repeated medians. *Biometrika* **69**,
242–244.

Tsai, W.-Y. (1990). Testing the assumption of independence of truncation time and
failure time. *Biometrika* **77**, 169–177.

---

## Appendix A. Summary of the null objects

For quick reference, under the clean model (M) and a $\mathcal W$-closed transport
with look-back $k$ and window $\mathcal W = \{r-k,\dots,r\}$. The **exact,
nuisance-free** objects are the window total and discriminant; the allocation law
carries the nuisance through $\pi_r = \mu_r/M_r(k)$ and is exact only when the null
means are known (oracle), approximate once they are estimated:

| object | definition | null law | status | sensitive to |
|---|---|---|---|---|
| window total $S_r(k)$ | $\sum_{j\in\mathcal W} R_j$ | $\operatorname{Poisson}(M_r(k))$ | **exact & transport-invariant** (Thm 1) | creation only |
| discriminant $\Delta_r(k)$ | $S_r(k) - M_r(k)$ | mean $0$ under any batch | **exact** (Thm 1) | creation only |
| release count $R_r \mid S_r(k){=}n$ | — | $\operatorname{Binomial}(n, \pi_r)$, $\pi_r=\mu_r/M_r(k)$ | exact given $\pi_r$ (Thm 2); **feasible test approximate** (§6) | transport toward $r$ |
| deficit $W_r(k)$ | $\sum_{j<r}(\mu_j - R_j)$ | contrast of the multinomial; unweighted form motivated in §5.2 | heuristic optimality | transport |

The transport test rejects for $R_r$ in the upper binomial tail (5.2). Its similarity
holds for every $(\lambda, g_D)$ **only in the oracle version** with $\pi_r$ known
(Proposition 1); the feasible version substitutes an external estimate $\widehat\pi_r$
and is approximately similar, with second-order size distortion (§6). Over-dispersion
replaces the binomial/Poisson tails by robust normal tails standardised by
$\widehat\phi$ (§7); a scheduled period is divided out by robust phase factors (§8);
and multiplicity across report times is controlled by Benjamini–Hochberg (§9).
