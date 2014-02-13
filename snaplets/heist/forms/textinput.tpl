<!-- wrap our form template with the primary base template -->
<apply template="base">

  <!-- the form splice to be processed by digestive-functors -->
  <textInput action="/textinput">

    <dfLabelError ref="textinput">Text:</dfLabelError>
    <dfInputText ref="textinput" checkerror="textinput" />
    <dfErrorsInline ref="textinput" />

    <dfInputSubmit value="Submit" />

  </textInput>

  <!-- the result/code/template tabs unique to this form -->
  <textInputTabs />

</apply>
