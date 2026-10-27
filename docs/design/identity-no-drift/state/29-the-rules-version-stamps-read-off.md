# 29. The rules version stamps read off the classpath

- **Where:** `ClasspathResource`
- **Verdict:** **(b) fixed**

Written under `services/identity/` (`ClasspathResource.Stamps`), and a missing one throws: at the root, an older build's copy in common's output came first on the worker's classpath and would have frozen the versions.
