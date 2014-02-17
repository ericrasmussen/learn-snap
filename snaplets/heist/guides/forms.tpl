<apply template="base">

  <h3>Forms and Formlets and Functors</h3>
  <h5>...oh my</h5>

  <ul class="orbit-content" data-orbit>
    <li data-orbit-slide="headline-1">
      <div>
        <h3>Represent a form in code</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-2">
      <div>
        <h3>Add validation steps to check input</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-3">
      <div>
        <h3>Get a code to form field mapping for free</h3>
      </div>
    </li>
  </ul>

  <h3>Formlets</h3>

  <p>
    User interaction via HTML forms is always tricky. Typically the goal is to
    take some representation of data in code, represent it as a form
    to the user, and validate/translate the user's input back to the internal
    representation. To top it off: you have no way to guarantee that the form
    field IDs can map to the individual form field handlers.
  </p>
  <p>
    Formlets are a language-agnostic abstraction designed to address this. The
    gist of it is you can define forms in code that will be responsible for
    automatically generating unique IDs for the rendered form fields, statically
    guaranteeing that the user input for those fields can be associated with
    the code that handles each field. If you'd like to learn more about the
    research behind it, the original paper and examples are available on
    <a href="http://groups.inf.ed.ac.uk/links/formlets/">The Essence of Form
      Abstraction</a>.
  </p>

  <h3>digestive-functors</h3>

  <p>
    The <a href="http://hackage.haskell.org/package/digestive-functors">
    digestive-functors</a> form library is one implementation of formlets. Don't
    be alarmed by the name! It's actually very simple to use. It also expands on
    the formlets idea by making it easier to add field labels and conditionally
    displaying validation errors when rendering the form.
  </p>

  <h3>Digestively Functored Compiled Heist Snaplets</h3>

  <p>
    OK, that's not a real name for anything. But one of the goals for this site
    is to show how you can use Compiled Heist for templating in a Snap
    application, while using digestive-functors for defining, rendering, and
    validating forms.
  </p>

  <p>
    Although it takes very little code to put them all together, very few
    examples existed online prior to 2014. One of the original motivations for
    this site was to demonstrate how they work, so you'll find several examples
    in the right sidebar underneath the Forms heading. Each of the examples will
    show how to define a Snap handler, Heist splices, and digestive-functors
    forms in a Haskell module, along with the corresponding Heist template.
  </p>

  <p>
    The good news is several libraries already exist to make use of these
    libraries together, and Text.Digestive.Heist comes pre-equipped with Heist
    splice bindings for rendering form fields. In the example templates you'll
    see nodes such as &lt;dfLabel&gt; and &lt;dfInputText&gt;. We have full
    control over where we place them and how we style them, but
    digestive-functors controls how to uniquely label the fields and map user
    input to the form's Haskell representation. You can see more of the built-in
    field definitions in the docs for
    <a href="http://hackage.haskell.org/package/digestive-functors-heist-0.8.3.0/docs/Text-Digestive-Heist-Compiled.html">
    Text.Digestive.Heist.Compiled</a>.
  </p>

</apply>
