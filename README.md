# haskellcard

Hypercard-like game engine. Alpha/testing.

## Running

### Using the release binary

Download one of the example ``.carddeck.zip` files to run. You can even extract the zip
and run the folder and modify things, but I think it'll check for a zip first.

Download the release binary which I think is a standalone Linux binary which maybe runs on
all kinds of Linux machines and then maybe use it like this:

```
SDL_RENDER_DRIVER=software haskellcard deckquest
```

I haven't tested this yet.

### Run with cabal

You may need to install these dependencies (hopefully I included all of the required below):

```
sudo apt-get install libsdl2-dev
sudo apt-get install libsdl2-ttf-dev
sudo apt-get install libsdl2-image-dev
sudo apt install libsdl2-mixer-dev
```

Then try running a *card deck* with this command:

```
SDL_RENDER_DRIVER=software cabal run haskellcard-exe -- examples/deckquest
```

## Nix

I recommend using [the Nix Packages (Nixpkgs)
collection](https://github.com/NixOS/nixpkgs) which aims to give us the same development
environment and reproducible builds.

Run with something like:

```
SDL_RENDER_DRIVER=software nix run .#haskellcard -- examples/deckquest
```

### Tests and Environment

Use the development I usse--enter with the `nix develop` command.

Run tests with `cabal test`.

## Thanks

Using these sprites of tux https://opengameart.org/content/tux-kyrodian-legends-style

This music https://opengameart.org/content/cave-theme
