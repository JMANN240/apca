[<img src="https://api.gitsponsors.com/api/badge/img?id=257476648" height="20">](https://api.gitsponsors.com/api/badge/link?p=u1XjXfeiH/1FQ2+aW07mOyew0k+ay0/65AtYCi/YsPxuOaYJiisYYsZi0DyDazaa)
[![pipeline](https://github.com/d-e-s-o/apca/actions/workflows/test.yml/badge.svg?branch=main)](https://github.com/d-e-s-o/apca/actions/workflows/test.yml)
[![coverage](https://codecov.io/gh/d-e-s-o/apca/branch/main/graph/badge.svg)](https://codecov.io/gh/d-e-s-o/apca)
[![crates.io](https://img.shields.io/crates/v/apca.svg)](https://crates.io/crates/apca)
[![Docs](https://docs.rs/apca/badge.svg)](https://docs.rs/apca)
[![rustc](https://img.shields.io/badge/rustc-1.63+-blue.svg)](https://blog.rust-lang.org/2022/08/11/Rust-1.63.0.html)

apca
====

- [Documentation][docs-rs]
- [Changelog](CHANGELOG.md)

**apca** is a library for interacting with the Alpaca API at
[alpaca.markets][]. The crate is entirely written in Rust and exposes a
fully async API based on the native `async`/`await` language feature.

The crate provides access to the majority of functionality provided by
Alpaca, including, but not limited to:
- inquiring of account information
- changing of the account configuration
- retrieving of past account activity
- accessing the market clock
- submitting, changing, listing, and canceling orders
- listing and closing open positions
- listing and retrieving general asset information
- streaming of order updates over WebSocket
- historic market data retrieval through Alpaca's Data API
- real time market data streaming via Alpaca's websocket API

For convenient command-line based access to the API, please use
[`apcacli`][apcacli].


Usage
-----

The following example illustrates how to create a `Client` object and
then submit a limit order for `AAPL` with a limit price of USD 100:
```rust
let api_info = ApiInfo::from_env().unwrap();
let client = Client::new(api_info);

let request = order::CreateReqInit {
  type_: Type::Limit,
  limit_price: Some(Num::from(100)),
  ..Default::default()
}
.init("AAPL", Side::Buy, order::Amount::quantity(1));

let order = client
  .issue::<order::Create>(&request)
  .await
  .unwrap();
```

The returned `order` object can subsequently be inspected to find out
details about the order (such as its ID). The full example is available
[here][example-order].

Please refer to the full [documentation][docs-rs] for more details.


[example-order]: examples/order.rs
[docs-rs]: https://docs.rs/crate/apca
[alpaca.markets]: https://alpaca.markets
[apcacli]: https://crates.io/crates/apcacli
[polyio]: https://crates.io/crates/polyio
