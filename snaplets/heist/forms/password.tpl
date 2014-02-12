<apply template="formwrapper">


  <p>
    This is a fake login form for demonstration purposes. Please do not supply
    a real login and password.
  </p>

    <password action="/password">

        <dfLabelError ref="username">Login:</dfLabelError>
        <dfInputText  ref="username" checkerror="username" />
        <dfErrorsInline ref="username" />

        <dfLabelError ref="password">Password:</dfLabelError>
        <dfInputPassword ref="password" checkerror="password" />
        <dfErrorsInline ref="password" />

        <dfInputSubmit value="Submit" />

    </password>

</apply>
