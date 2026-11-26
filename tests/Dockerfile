# Test-resource bundle image for snippet-repo CI (see README "CI/CD Pipeline").
# Build context: this directory, containing a freshly built
# resources.tar.xz.asc (run `snip bundle <resource/shared dir>
# <image-passphrase-file>` first, then copy the bundle here):
#   docker build -t hhhpa/snippet-resources:t1263 -f tests/Dockerfile tests/
#   docker push hhhpa/snippet-resources:t1263
FROM scratch
COPY resources.tar.xz.asc /tests/resources.tar.xz.asc
LABEL org.opencontainers.image.title="snippet-test-resources"
