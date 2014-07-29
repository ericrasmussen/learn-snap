<apply template="base">

  <h2>Compiled Runtime Values</h2>

  <p>
    Queries a fake Fizzbuzz Service (returning IO (Maybe Text)) for each Int in
    a range of Ints, inspects the returned value, and chooses nothing.tpl in the
    Nothing case and just_value.tpl in the Just case.
  </p>

  <table>
    <thead>
      <tr>
        <th>Index</th>
        <th>Value</th>
      </tr>
    </thead>
    <tbody>

      <fizzbuzzes>

        <tr>
          <td>
            <fizzIndex/>
          </td>
          <td>
            <maybeFizzValue/>
          </td>
        </tr>

      </fizzbuzzes>

    </tbody>
  </table>

  <runtimeTabs/>

</apply>
