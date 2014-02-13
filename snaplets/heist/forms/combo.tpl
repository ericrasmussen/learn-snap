<!-- wrap our form template with the primary base template -->
<apply template="base">

  <!-- the form splice to be processed by digestive-functors -->
  <combo action="/combo">

    <dfLabelError ref="combo">Choice of tea:</dfLabelError>
    <dfInputSelect ref="combo" checkerror="combo" />
    <dfErrorsInline ref="combo" />

    <dfInputSubmit value="Submit" />

  </combo>

  <!-- the result/code/template tabs unique to this form -->
  <comboTabs />

</apply>
