# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

  * Fullscreen feature and hopefully window rescaling support
  * Key support

### Fixed

  * Issue #2 on GitHub: if loading the carddeck as a zip the temporary directory the zip
    was extracted into would be left, which would lead to errors when the card deck was
    loaded again and it tried to create a temporary directory to extract to and the
    directory already exists. Resolved by both checking if the temporary directory already
    exists first (and removing if it does) and also cleaning up the temporary directory
    before quitting, basically

### Changed

  * Using new record syntax, meaning shorter names?

## [0.1.0.0] - 2023-12-25 (Christmas)

First release, initially released as a private invite-only release on Christmas Day.

[unreleased]: https://github.com/someodd/haskellcard/compare/v0.1.0.0...HEAD
[0.1.0.0]: https://github.com/someodd/haskellcard/release/v0.1.0.0
