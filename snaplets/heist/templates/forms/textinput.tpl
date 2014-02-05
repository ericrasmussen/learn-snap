<apply template="base">

  <div class="row">
    <textInputForm action="/textinput">

        <dfLabelError ref="textinput" class="inline">Text: </dfLabelError>
        <dfInputText ref="textinput" checkerror="textinput" />
        <dfSmallErrors ref="textinput" class="error" />

      <dfInputSubmit value="Submit" />

    </textInputForm>
  </div>

  <apply template="tabs" />

</apply>
