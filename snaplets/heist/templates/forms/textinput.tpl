<apply template="base">

  <div class="row">
    <textInputForm action="/textinput">

        <dfLabelError ref="textinput">Text: </dfLabelError>
        <dfInputText ref="textinput" checkerror="textinput" />
        <dfErrorsInline ref="textinput" />

      <dfInputSubmit value="Submit" />

    </textInputForm>
  </div>

  <apply template="tabs" />

</apply>
