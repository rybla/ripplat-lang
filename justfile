clean:
  -rm -rf output/Ripplat.*


build:
  bun run build

rebuild: clean build

test:
  bun run test
