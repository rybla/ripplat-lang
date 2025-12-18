# Clean any existing build artifacts
clean:
  -rm -rf output/Ripplat.*

# Build the entire project
build:
  bun run build

# Clean any existing build artifacts, then build.
rebuild: clean build

# Run all tests
test:
  bun run test

