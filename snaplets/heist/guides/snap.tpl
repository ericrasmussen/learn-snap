<apply template="base">

  <h3>Meet the Snaplets</h3>

  <ul class="orbit-content" data-orbit>
    <li data-orbit-slide="headline-1">
      <div>
        <h3>Create request/response handlers</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-2">
      <div>
        <h3>Bind them to routes</h3>
      </div>
    </li>
    <li data-orbit-slide="headline-3">
      <div>
        <h3>Share/reuse code with Snaplets</h3>
      </div>
    </li>
  </ul>

  <h3>The Really High Level</h3>

  <p>
    The Snap Framework is a bundled HTTP server and set of Haskell libraries
    that make it easy to build web applications. You write code that inspects
    an incoming HTTP request and sends a response (whether it's HTML, XML, JSON,
    etc).
  </p>

  <p>
    Sounds pretty typical for a web framework, right?
  </p>

  <p>
    One of Snap's defining features is the concept of Snaplets. They are similar
    in spirit to plugins, extensions, or subsites. You can bundle up reusable
    components (think: database API and connection pooling, caching, sessions,
    template engines), and add them to your Snap config. At that point you can
    access that functionality in your Snap handlers.
  </p>

  <p>
    This makes it especially easy to add and remove modular components without
    having to change the type of every handler, and it also encourages
    separating out code into reusable components that you can share with others.
  </p>

  <h3>More on Snaplets</h3>

  <p>
    One of the best Snaplet examples is the Heist Snaplet, which is used to
    make Heist available to your Snap handlers when rendering HTML5 or XML
    responses. Of course, you don't have to use Heist to use Snap, but it comes
    with Snap and demonstrates the ease of use and integration.
  </p>

  <p>
    You can learn more about Heist <a href="/guides/heist">here</a>, or check
    out the many other <a href="http://snapframework.com/snaplets">Snaplets</a>
    by the core team and 3rd parties.
  </p>

  <h3>Getting Started</h3>

  <p>
    This site is built using Snap, and you can view all the code in its
    <a href="https://github.com/ericrasmussen/digestive-heist-demos">
    GitHub repo</a>. After you see the basics and get a feel for what it can do,
    head on over to the <a href="http://snapframework.com/">official site</a>
    to find resources and docs on learning more.
  </p>

</apply>

