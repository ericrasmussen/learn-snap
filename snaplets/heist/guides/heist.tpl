<apply template="base">

  <h3>How to Heist</h3>

  <ul class="orbit-content" data-orbit>
    <li data-orbit-slide="headline-1">
      <div>
        <h3>Create valid XML or HTML5 markup</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-2">
      <div>
        <h3>Bind Haskell code to DOM nodes</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-3">
      <div>
        <h3>Profit!</h3>
      </div>
    </li>
  </ul>

  <h3>Heist: The Templates</h3>

  <p>
    Heist templates are HTML5 or XML files with a .tpl extension.
  </p>

  <p>
    Yep. It's that simple. Create valid markup, put it in a file, and you're
    good to go. No control flow or special syntax, making it very designer
    friendly, easy to read, and easy to debug.
  </p>

  <h3>Heist: The Library</h3>

  <p>
    A powerful Haskell library with a rich API for manipulating nodes from the
    HTML5 or XML .tpl files.
  </p>

  <p>
    The Heist library gives you a number of powerful ways to read and manipulate
    templates. You can bind Haskell functions to nodes. The functions can
    access the node's attributes and inner nodes, so your control flow is 100%
    Haskell driven. This makes it easy to conditionally render or repeat
    snippets.
  </p>

  <h3>Heist: The Snaplet</h3>

  <p>
    The Heist snaplet gives you an easy way to configure and use Heist templates
    and the Heist library inside your Snap application.
  </p>

  <p>
    This site only uses Compiled Heist, which requires you to bind your Haskell
    functions to nodes in your top level Heist config. The binding of template
    nodes to Haskell code is called a splice. Each splice that's bound
    can make decisions based on the node attributes, the inner nodes, and
    runtime values. Any data you can obtain from a Snap handler, like
    database queries, values in the current session, etc. can be passed in to
    your bound functions and used to render templates.
  </p>

  <p>
    One of the built-in splices is the &lt;apply&gt; node, which lets you call
    other templates via the template attribute: &lt;apply
    template="your-template"&gt;.  You'll be able to define your own nodes in a
    similar way, and we have many examples in the right sidebar demonstrating
    different types of flow control and form rendering.
  </p>

  <p>
    If it sounds limiting to have to bind your splices at the top level, it's
    not: within each splice you can nest any number of other splices. It would
    be possible (if a very bad idea!) to render your entire site from a single
    top level node, which internally would handle conditional rendering,
    repetition, runtime value substitution, etc. More realistically, you might
    have something like a top level &lt;userProfile&gt; splice that uses
    information about the current user to decide which templates to call and
    which other splices to include.
  </p>

  <h3>Heist: The Breakfast Cereal</h3>

  <p>
    You're probably getting the impression by now that Heist is more or less
    what you want it to be. It enforces a strict separation of view from
    logic, empowering you to write more type safe Haskell code specific to your
    application.
  </p>

  <p>
    We recommend checking out the examples in the right sidebar to get a feel
    for what you can do with Heist and what it looks like,
    then moving to
    the <a href="http://snapframework.com/docs/tutorials/heist"> Official
    Tutorial</a> to learn the fundamentals.
  </p>

</apply>
