<apply template="base">

  <h2>Conditional Text</h2>

  <table>
    <thead>
      <tr>
        <th>Title</th>
        <th>Author</th>
      </tr>
    </thead>
    <tbody>

      <conditionalText>
        <tr>
          <td>
            <a href="${urlA}"><titleA/></a>
          </td>
          <td>
            <maybeAuthorA/>
          </td>
        </tr>

        <tr>
          <td>
            <a href="${urlB}"><titleB/></a>
          </td>
          <td>
            <maybeAuthorB/>
          </td>
        </tr>
      </conditionalText>

    </tbody>
  </table>

  <conditionalTabs/>

</apply>
