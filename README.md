<h1>Linking physiological responses to environmental condition</h1>

<h2>Identifying thresholds of sublethal stress in benthic marine invertebrates</h2>

<p>This repository contains the analysis code associated with the manuscript:</p>

<p>
<strong>McGrath, E.C., Bennett, H., McMullin, R.M. &amp; Atalah, J.</strong><br>
<em>Linking physiological responses to environmental condition: identifying thresholds of sublethal stress in benthic marine invertebrates.</em>
</p>

<h2>Overview</h2>

<p>
This study investigates whether sublethal physiological responses in benthic marine
invertebrates can be linked to established measures of environmental condition and
used to identify candidate physiological thresholds.
</p>

<p>
Using organic enrichment associated with salmon aquaculture as a case study,
complementary laboratory and field experiments were conducted with three benthic
invertebrate species:
</p>

<ul>
  <li>Horse mussel (<em>Atrina zelandica</em>)</li>
  <li>New Zealand scallop (<em>Pecten novaezelandiae</em>)</li>
  <li>Apricot brachiopod (<em>Neothyris lenticularis</em>)</li>
</ul>

<p>
Physiological responses were assessed across multiple domains, including fatty acid
composition, respiration, oxidative stress biomarkers, histopathology, organismal
condition, and gene expression.
</p>

<p>
Environmental condition was represented using <strong>enrichment stage (ES)</strong>,
an established measure of benthic organic enrichment.
</p>

<h2>Analysis workflow</h2>

<ol>
  <li>
    <strong>Response screening and reduction</strong><br>
    Correlation and network analyses were used to identify redundancy among
    physiological response variables and select representative variables for
    subsequent analysis.
  </li>
  
  <li>
    <strong>Environmental response modelling</strong><br>
    Relationships between physiological responses and ES were modelled separately
    for each species-variable combination using generalised additive models (GAMs).
  </li>

  
  <li>
    <strong>Threshold estimation</strong><br>
    Candidate ES thresholds were estimated for significant physiological responses.
    For non-linear relationships, thresholds were identified using first derivatives
    of GAM smooths. For linear relationships, thresholds were identified relative to
    baseline variability in control conditions.
  </li>
</ol>

<p>
Laboratory- and field-derived thresholds were subsequently compared to assess the
consistency of physiological responses across experimental systems.
</p>

<h2>Software</h2>

<p>Analyses were conducted in <strong>R</strong>. Key packages include:</p>

<ul>
  <li><code>mgcv</code> — generalised additive models</li>
  <li><code>gratia</code> — GAM derivatives and interpretation</li>
  <li><code>dplyr</code> and other <code>tidyverse</code> packages — data manipulation and visualisation</li>
</ul>

<p>
Additional package dependencies are documented within the individual analysis scripts.
</p>

<h2>Reproducibility</h2>

<p>
Scripts are intended to be run in the order indicated by their filenames or directory
structure.
</p>

<p>
Where data cannot be made publicly available because of data ownership,
confidentiality, or other restrictions, this is identified in the relevant script
or data directory.
</p>

<p>
The repository provides the code required to reproduce the analytical workflow
described in the manuscript, subject to availability of the underlying datasets.
</p>

<h2>Citation</h2>

<p>If using the code or analytical workflow from this repository, please cite:</p>

<blockquote>
McGrath, E.C., Bennett, H., McMullin, R.M. &amp; Atalah, J.
<em>Linking physiological responses to environmental condition: identifying
thresholds of sublethal stress in benthic marine invertebrates.</em>
</blockquote>

<p>Full citation details will be added following publication.</p>
