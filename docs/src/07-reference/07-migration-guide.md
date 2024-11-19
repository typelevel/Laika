
Migration Guide
===============

The upgrade from 0.x to 1.x versions was not a feature release. The primary focus was to adjust the public API surface to allow it to evolve more easily in a way that is binary backwards-compatible.

It's recommended to first update to 0.19.5, if you haven't done this already, and address all deprecation warnings first. All deprecated APIs had been removed for 1.0.

If you are still using versions older than 0.19.x, you might want to check the migration guide for those versions first,
as this guide only focuses on the changes from 0.19 to 1.0.

For users who only use the library's configuration APIs the upgrade should be straightforward (in many cases a simple "Organize Imports" to adjust for the new package structure will be sufficient). Users who developed extensions might need further adjustments for some the breaking API changes.


New Organization ID
-------------------

The library is now published under the `org.typelevel` organization

All 0.x releases had been published under `org.planet42` which will no longer be used in the future.


Changed Behaviour for Including CSS/JS
--------------------------------------

This is a breaking change the compiler does not help with detecting. If you previously had custom CSS or JavaScript files auto-linked to your pages, you need to adjust your configuration.

The old default of using a global search path for automatically linking all CSS and JS files from the input sources is no longer active. Users need to explicitly configure at least one search path (via `.site.internalCSS` or `.site.internalJS`) for a resource scan to happen.

The old behaviour was often unexpected (and users had to search for ways to disable it) and also came with the risk of overlapping search paths where a theme or extension adds their own resources. The new API allows more control in general (e.g. setting a condition per document for whether resources are linked or not).

See the [corresponding PR](https://github.com/typelevel/Laika/pull/511) for additional details.

When not using the Helium theme, users are required to include their own custom template anyway,
so the simplest option to migrate might be to just hard-code all file inclusions in the `<head>` section
of the template or, if more flexibility is required, use some of the templating features described in [Creating Templates].


Breaking API Changes
--------------------

### New package structure

[Full PR Description](https://github.com/typelevel/Laika/pull/533)

The public API had been cleaned up, since it had become fragmented after many types had been made private for 1.0. In laika-core the number of top-level packages has been reduced from 14 to 5. Apart from moving a lot of classes to new packages, many ADT type members have also been moved to their companion for better API navigation. In most cases a simple "Organize Imports" should suffice. See the PR for details.

### Reduced public API surface

[Full PR Description](https://github.com/typelevel/Laika/pull/452)

The number of public types has been reduced by about a quarter of the API surface compared to 0.19.x. Like many other changes for 1.0 the main motivation was to more easily evolve the library in a binary backwards-compatible way. The removed APIs were so low-level that very few users should be affected by this change.

### Many case classes became regular classes

[Full PR Description](https://github.com/typelevel/Laika/pull/482)

Past experience showed that classes used for configuration purposes evolve more frequently than most other APIs. For this reason many have been converted to traits with private implementation types. The `apply` methods have been reduced to required parameters only. Optional properties can be set with the common `withXX` pattern. Types which don't have any required properties usually offer an `XX.defaults` or `XX.empty` entry point depending on whether it is pre-populated by the library or not.

### Simplified Formatter APIs

[Full PR Description](https://github.com/typelevel/Laika/pull/523)

Concrete formatter types like `HTMLFormatter` or `FOFormatter` are private now and their API has been unified 
under the two traits `Formatter` and `TagFormatter`. 
Several method signatures in `TagFormatter` have also been simplified for more concise renderer implementations.
See the PR for details.

### Removal of all deprecated APIs

[Full PR Description](https://github.com/typelevel/Laika/pull/429)

For a more convenient upgrade, compile against 0.19.5 to see all deprecation warnings before migrating.
