<apply template="base">

  <h2>Looping</h2>

  <p>
    Repeats a snippet once for each color in our list (red, green, blue).
  </p>

  <table>
    <thead>
      <tr>
        <th>Name</th>
        <th>Sample</th>
      </tr>
    </thead>
    <tbody>

      <primaryColors>

        <tr>
          <td>
            <colorName/>
          </td>
          <td style="background-color:${colorHex};">
          </td>
        </tr>

      </primaryColors>

    </tbody>
  </table>

  <loopTabs/>

</apply>
