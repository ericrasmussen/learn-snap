<!-- wrap our form template with the primary base template -->
<apply template="base">

  <p>
    This is a fake login form for demonstration purposes. Please do not supply
    a real login and password.
  </p>

  <!-- the form splice to be processed by digestive-functors -->
  <password action="/password">

    <dfLabelError ref="username">Login:</dfLabelError>
    <dfInputText  ref="username" checkerror="username" />
    <dfErrorsInline ref="username" />

    <dfLabelError ref="password">Password:</dfLabelError>
    <dfInputPassword ref="password" checkerror="password" />
    <dfErrorsInline ref="password" />

    <dfInputSubmit value="Submit" />

  </password>

  <!-- the result/code/template tabs unique to this form -->
  <passwordTabs />

</apply>
