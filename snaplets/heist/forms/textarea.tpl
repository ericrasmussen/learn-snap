<!-- wrap our form template with the primary base template -->
<apply template="base">

  <!-- the form splice to be processed by digestive-functors -->
  <textArea action="/textarea">

    <dfLabelError ref="textarea">Text Area:</dfLabelError>
    <dfInputTextArea style="min-height:200px;" ref="textarea" checkerror="textarea" />
    <dfErrorsInline ref="textarea" />

    <dfInputSubmit value="Submit" />

  </textArea>

  <!-- the result/code/template tabs unique to this form -->
  <textAreaTabs />

</apply>
