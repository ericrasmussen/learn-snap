<!doctype html>
<!--[if IE 9]><html class="lt-ie10" lang="en" > <![endif]-->
<html class="no-js" lang="en" data-useragent="Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.2; Trident/6.0)">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Digestive Functor Demos in Snap</title>


    <meta name="description" content="Form demos using the Digestive Functors library with Snap and Heist" />

    <meta name="copyright" content="Chromatic Leaves. Copyright (c) 2013" />

    <link rel="stylesheet" href="/assets/foundation/css/foundation.css" />
    <script src="/assets/foundation/js/modernizr.js"></script>
  </head>
  <body>

<!-- Nav Bar -->

  <div class="row">
    <div class="large-12 columns">
      <div class="nav-bar right">
       <ul class="button-group">
         <li><a href="http://snapframework.com/" class="button">Snap</a></li>
         <li><a href="http://snapframework.com/docs/tutorials/heist" class="button">Heist</a></li>
         <li><a href="http://hackage.haskell.org/package/digestive-functors" class="button">Forms</a></li>
        </ul>
      </div>
      <h1>Digestive Heist Demos</h1>
      <hr />
    </div>
  </div>

  <!-- End Nav -->


  <!-- Main Page Content and Sidebar -->

  <div class="row">

    <!-- Main Blog Content -->
    <div class="large-9 columns" role="content">


      <apply-content/>


    </div>

    <!-- End Main Content -->


    <!-- Sidebar -->

    <aside class="large-3 columns">

      <h5>Demos</h5>
      <ul class="side-nav">
        <li><a href="#">News</a></li>
        <li><a href="#">Code</a></li>
        <li><a href="#">Design</a></li>
        <li><a href="#">Fun</a></li>
        <li><a href="#">Weasels</a></li>
      </ul>

    </aside>

    <!-- End Sidebar -->
  </div>

  <!-- End Main Content and Sidebar -->


  <!-- Footer -->

  <footer class="row">
    <div class="large-12 columns">
      <hr />
      <div class="row">
        <div class="large-8 columns">
          <p>&copy; 2009 - 2014 Chromatic Leaves</p>
        </div>
      </div>
    </div>
  </footer>

    <script src="/assets/foundation/js/jquery.js"></script>
    <script src="/assets/foundation/js/foundation.min.js"></script>
    <script>
      $(document).foundation();

      var doc = document.documentElement;
      doc.setAttribute('data-useragent', navigator.userAgent);
    </script>
  </body>
</html>
