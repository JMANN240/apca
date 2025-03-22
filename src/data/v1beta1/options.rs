// Copyright (C) 2021-2025 The apca Developers
// SPDX-License-Identifier: GPL-3.0-or-later

use std::collections::HashMap;

use chrono::DateTime;
use chrono::naive::NaiveDate;
use chrono::Utc;

use num_decimal::Num;

use regex::Regex;
use serde::Deserialize;
use serde::Serialize;
use serde_urlencoded::to_string as to_query;

use crate::data::DATA_BASE_URL;
use crate::Str;


/// Represents a contract
#[derive(Debug)]
pub struct Contract {
  /// The name of the underlying symbol.
  pub underlying_symbol: String,
  /// The date on which this contract expires.
  pub expiration_date: NaiveDate,
  /// The type of option that this contract represents.
  pub option_type: OptionType,
  /// The strike price of the contract.
  pub strike_price: Num
}


impl Contract {
  /// Extracts contract information from a contract name.
  pub fn parse(name: &str) -> Option<Self> {
    let re = Regex::new(r"^(?<symbol>\D{1,6})(?<year>\d{2})(?<month>\d{2})(?<day>\d{2})(?<type>C|P)(?<millidollars>\d{8})").unwrap();
    match re.captures(name) {
      Some(captures) => {
        let year = 2000 + captures["year"].parse::<i32>().unwrap();
        let month = captures["month"].parse::<u32>().unwrap();
        let day = captures["day"].parse::<u32>().unwrap();
        let expiration_date = NaiveDate::from_ymd_opt(year, month, day).unwrap();
        Some(Contract {
          underlying_symbol: captures["symbol"].to_string(),
          expiration_date: expiration_date,
          option_type: OptionType::try_from(&captures["type"]).unwrap(),
          strike_price: Num::new(captures["millidollars"].parse::<u32>().unwrap(), 1000)
        })
      },
      None => None
    }
  }
}


/// An enumeration of the various supported feeds.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum Feed {
  /// Official OPRA feed.
  #[serde(rename = "opra")]
  OPRA,
  /// Free indicative feed.
  #[serde(rename = "indicative")]
  Indicative,
}


/// An enumeration of the various supported option types.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Serialize)]
#[non_exhaustive]
pub enum OptionType {
  /// Call option.
  #[serde(rename = "call")]
  Call,
  /// Put option.
  #[serde(rename = "put")]
  Put,
}


/// Represents an error with parsing the option type
#[derive(Clone, Copy, Debug)]
pub struct OptionTypeParsingError;


impl TryFrom<&str> for OptionType {
  type Error = OptionTypeParsingError;

  fn try_from(string: &str) -> Result<Self, Self::Error> {
    match string {
      "C" => Ok(Self::Call),
      "P" => Ok(Self::Put),
      _ => Err(OptionTypeParsingError),
    }
  }
}


/// A GET request to be issued to the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Eq, PartialEq, Serialize)]
pub struct ListReq {
  /// The underlying symbol for which to retrieve market data.
  #[serde(skip)]
  pub underlying_symbol: String,
  /// The data feed to use.
  ///
  /// Defaults to OPRA if the user has a subscription, otherwise indicative.
  #[serde(rename = "feed", skip_serializing_if = "Option::is_none")]
  pub feed: Option<Feed>,
  /// The maximum number of snapshots to be returned.
  ///
  /// It can be between 1 and 1000. Defaults to 100 if the provided
  /// value is None.
  #[serde(rename = "limit", skip_serializing_if = "Option::is_none")]
  pub limit: Option<usize>,
  /// Filter to contracts that were updated since this timestamp.
  #[serde(rename = "updated_since", skip_serializing_if = "Option::is_none")]
  pub updated_since: Option<DateTime<Utc>>,
  /// If provided we will pass a page token to continue where we left off.
  #[serde(rename = "page_token", skip_serializing_if = "Option::is_none")]
  pub page_token: Option<String>,
  /// Filter contracts by type.
  #[serde(rename = "type", skip_serializing_if = "Option::is_none")]
  pub option_type: Option<OptionType>,
  /// Filter to contracts with a strike price greater than or equal to this value.
  #[serde(rename = "strike_price_gte", skip_serializing_if = "Option::is_none")]
  pub strike_price_gte: Option<Num>,
  /// Filter to contracts with a strike price less than or equal to this value.
  #[serde(rename = "strike_price_lte", skip_serializing_if = "Option::is_none")]
  pub strike_price_lte: Option<Num>,
  /// Filter to contracts with an expiration date equal to this date.
  #[serde(rename = "expiration_date", skip_serializing_if = "Option::is_none")]
  pub expiration_date: Option<NaiveDate>,
  /// Filter to contracts with an expiration date equal to or after this time.
  #[serde(rename = "expiration_date_gte", skip_serializing_if = "Option::is_none")]
  pub expiration_date_gte: Option<NaiveDate>,
  /// Filter to contracts with an expiration date equal to or before this time.
  #[serde(rename = "expiration_date_lte", skip_serializing_if = "Option::is_none")]
  pub expiration_date_lte: Option<NaiveDate>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// A helper for initializing [`ListReq`] objects.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct ListReqInit {
  /// See `ListReq::limit`.
  pub limit: Option<usize>,
  /// See `ListReq::feed`.
  pub feed: Option<Feed>,
  /// See `ListReq::page_token`.
  pub page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  pub _non_exhaustive: (),
}

impl ListReqInit {
  /// Create a [`ListReq`] from a `ListReqInit`.
  #[inline]
  pub fn init<S>(
    self,
    symbol: S,
  ) -> ListReq
  where
    S: Into<String>,
  {
    ListReq {
      underlying_symbol: symbol.into(),
      expiration_date_gte: None,
      expiration_date_lte: None,
      limit: self.limit,
      feed: self.feed,
      page_token: self.page_token,
      updated_since: None,
      strike_price_gte: None,
      strike_price_lte: None,
      option_type: None,
      expiration_date: None,
      _non_exhaustive: (),
    }
  }
}


/// An option snapshot as returned by the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct Snapshot {
  /// The bars of this snapshot.
  #[serde(rename = "dailyBar")]
  pub daily_bar: Option<Bar>,
  /// The greeks of this snapshot.
  #[serde(rename = "greeks")]
  pub greeks: Option<Greeks>,
  /// The Black-Scholes implied volatility.
  #[serde(rename = "impliedVolatility")]
  pub implied_volatility: Option<Num>,
  /// The best bid and ask information for this snapshot.
  #[serde(rename = "latestQuote")]
  pub latest_quore: Option<LatestQuote>,
  /// The most recent trade.
  #[serde(rename = "latestTrade")]
  pub latest_trade: Option<LatestTrade>,
  /// OHLC aggregate of all the trades in a given interval.
  #[serde(rename = "minuteBar")]
  pub minute_bar: Option<Bar>,
  /// OHLC aggregate of all the trades in a given interval.
  #[serde(rename = "prevDailyBar")]
  pub prev_daily_bar: Option<Bar>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// A otion bar as returned by the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct Bar {
  /// The beginning time of this bar.
  #[serde(rename = "t")]
  pub time: DateTime<Utc>,
  /// The open price.
  #[serde(rename = "o")]
  pub open: Num,
  /// The close price.
  #[serde(rename = "c")]
  pub close: Num,
  /// The highest price.
  #[serde(rename = "h")]
  pub high: Num,
  /// The lowest price.
  #[serde(rename = "l")]
  pub low: Num,
  /// The trading volume.
  #[serde(rename = "v")]
  pub volume: usize,
  /// The volume weighted average price.
  #[serde(rename = "vw")]
  pub weighted_average: Num,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// A collection of greeks as returned by the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct Greeks {
  /// The delta.
  #[serde(rename = "delta")]
  pub delta: Num,
  /// The gamma.
  #[serde(rename = "gamma")]
  pub gamma: Num,
  /// The theta.
  #[serde(rename = "theta")]
  pub theta: Num,
  /// The vega.
  #[serde(rename = "vega")]
  pub vega: Num,
  /// The rho.
  #[serde(rename = "rho")]
  pub rho: Num,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// Data about the last quote on an option as returned by the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct LatestQuote {
  /// The beginning time of this bar.
  #[serde(rename = "t")]
  pub time: DateTime<Utc>,
  /// The bid exchange.
  #[serde(rename = "bx")]
  pub bid_exchange: String,
  /// The bid price.
  #[serde(rename = "bp")]
  pub bid_price: Num,
  /// The bid size.
  #[serde(rename = "bs")]
  pub bid_size: u32,
  /// The ask exchange.
  #[serde(rename = "ax")]
  pub ask_exchange: String,
  /// The ask price.
  #[serde(rename = "ap")]
  pub ask_price: Num,
  /// The ask size.
  #[serde(rename = "as")]
  pub ask_size: u32,
  /// The quote condition.
  #[serde(rename = "c")]
  pub quote_condition: String,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// Data about the last trade of an option as returned by the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq)]
pub struct LatestTrade {
  /// The time of this trade.
  #[serde(rename = "t")]
  pub time: DateTime<Utc>,
  /// The exchange.
  #[serde(rename = "x")]
  pub exchange: String,
  /// The price.
  #[serde(rename = "p")]
  pub price: Num,
  /// The size.
  #[serde(rename = "s")]
  pub size: u32,
  /// The trade condition.
  #[serde(rename = "c")]
  pub trade_condition: String,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


/// A collection of option snapshots as returned by the API. This is one page of
/// snapshots.
#[derive(Debug, Deserialize, Eq, PartialEq)]
pub struct Snapshots {
  /// The list of returned snapshots.
  #[serde(rename = "snapshots")]
  pub snapshots: HashMap<String, Snapshot>,
  /// The token to provide to a request to get the next page of snapshots for
  /// this request.
  #[serde(rename = "next_page_token")]
  pub next_page_token: Option<String>,
  /// The type is non-exhaustive and open to extension.
  #[doc(hidden)]
  #[serde(skip)]
  pub _non_exhaustive: (),
}


Endpoint! {
  /// The representation of a GET request to the /v1beta1/options/snapshots/{underlying_symbol} endpoint.
  pub List(ListReq),
  Ok => Snapshots, [
    /// The market data was retrieved successfully.
    /* 200 */ OK,
  ],
  Err => ListError, [
    /// A query parameter was invalid.
    /* 400 */ BAD_REQUEST => InvalidInput,
  ]

  fn base_url() -> Option<Str> {
    Some(DATA_BASE_URL.into())
  }

  fn path(input: &Self::Input) -> Str {
    format!("/v1beta1/options/snapshots/{}", input.underlying_symbol).into()
  }

  fn query(input: &Self::Input) -> Result<Option<Str>, Self::ConversionError> {
    Ok(Some(to_query(input)?.into()))
  }
}


#[cfg(test)]
mod tests {
  use super::*;

  use http_endpoint::Endpoint;

  use serde_json::from_str as from_json;

  use test_log::test;

  use crate::api_info::ApiInfo;
  use crate::Client;


  #[test]
  fn parse_option_name() {
    let contract = Contract::parse("INTC250321C00037500").unwrap();

    assert_eq!(&contract.underlying_symbol, "INTC");
    assert_eq!(&contract.expiration_date, &NaiveDate::from_ymd_opt(2025, 3, 21).unwrap());
    assert_eq!(&contract.strike_price, &Num::new(375, 10));
    assert_eq!(&contract.option_type, &OptionType::Call);
  }


  /// Verify that we can properly parse a reference bar response.
  #[test]
  fn parse_reference_snapshots() {
    let response = r#"{
  "next_page_token": "SU5UQzI1MDMyMVAwMDA0MjUwMA==",
  "snapshots": {
    "INTC250321C00037500": {
      "dailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 2,
        "o": 0.01,
        "t": "2025-03-20T04:00:00Z",
        "v": 30,
        "vw": 0.01
      },
      "latestQuote": {
        "ap": 0,
        "as": 0,
        "ax": "?",
        "bp": 0,
        "bs": 0,
        "bx": "?",
        "c": "F",
        "t": "2025-03-21T12:30:03.856484096Z"
      },
      "latestTrade": {
        "c": "I",
        "p": 0.01,
        "s": 1,
        "t": "2025-03-20T16:38:16.314719232Z",
        "x": "J"
      },
      "minuteBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 1,
        "o": 0.01,
        "t": "2025-03-20T16:38:00Z",
        "v": 1,
        "vw": 0.01
      },
      "prevDailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 1,
        "o": 0.01,
        "t": "2025-03-18T04:00:00Z",
        "v": 1,
        "vw": 0.01
      }
    },
    "INTC250321C00065000": {
      "dailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 2,
        "o": 0.01,
        "t": "2025-03-17T04:00:00Z",
        "v": 2,
        "vw": 0.01
      },
      "latestQuote": {
        "ap": 0,
        "as": 0,
        "ax": "?",
        "bp": 0,
        "bs": 0,
        "bx": "?",
        "c": "F",
        "t": "2025-03-21T12:30:04.69317248Z"
      },
      "latestTrade": {
        "c": "f",
        "p": 0.01,
        "s": 1,
        "t": "2025-03-17T13:49:04.870710272Z",
        "x": "M"
      },
      "minuteBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 1,
        "o": 0.01,
        "t": "2025-03-17T13:49:00Z",
        "v": 1,
        "vw": 0.01
      },
      "prevDailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 2,
        "o": 0.01,
        "t": "2025-03-13T04:00:00Z",
        "v": 9,
        "vw": 0.01
      }
    },
    "INTC250321P00017500": {
      "dailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 3,
        "o": 0.01,
        "t": "2025-03-19T04:00:00Z",
        "v": 3,
        "vw": 0.01
      },
      "latestQuote": {
        "ap": 0,
        "as": 0,
        "ax": "?",
        "bp": 0,
        "bs": 0,
        "bx": "?",
        "c": "F",
        "t": "2025-03-21T12:30:04.243295488Z"
      },
      "latestTrade": {
        "c": "g",
        "p": 0.01,
        "s": 1,
        "t": "2025-03-19T15:31:56.292445952Z",
        "x": "M"
      },
      "minuteBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 3,
        "o": 0.01,
        "t": "2025-03-19T15:31:00Z",
        "v": 3,
        "vw": 0.01
      },
      "prevDailyBar": {
        "c": 0.01,
        "h": 0.01,
        "l": 0.01,
        "n": 2,
        "o": 0.01,
        "t": "2025-03-17T04:00:00Z",
        "v": 2,
        "vw": 0.01
      }
    }
  }
}"#;

    let res = from_json::<<List as Endpoint>::Output>(response).unwrap();
    let snapshots = res.snapshots;
    assert_eq!(snapshots.len(), 3);
    assert!(res.next_page_token.is_some())
  }

  /// Check that we can decode a response containing no snapshots correctly.
  #[test(tokio::test)]
  async fn no_snapshots() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let start = NaiveDate::from_ymd_opt(2011, 4, 6);
    let end = NaiveDate::from_ymd_opt(2011, 4, 6);
    let request = ListReq {
      underlying_symbol: "AAPL".to_string(),
      feed: None,
      limit: Some(2),
      updated_since: None,
      page_token: None,
      option_type: None,
      strike_price_gte: None,
      strike_price_lte: None,
      expiration_date: None,
      expiration_date_gte: start,
      expiration_date_lte: end,
      _non_exhaustive: () };

    let res = client.issue::<List>(&request).await.unwrap();
    assert_eq!(res.snapshots, HashMap::new())
  }

  /// Check that we can request historic snapshot data for an options chain.
  #[test(tokio::test)]
  async fn request_snapshots() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let request = ListReqInit {
      limit: Some(2),
      ..Default::default()
    }
    .init("AAPL");

    let res = client.issue::<List>(&request).await.unwrap();
    let snapshots = res.snapshots;

    assert_eq!(snapshots.len(), 2);
  }

  /// Verify that we can request data through a provided page token.
  #[test(tokio::test)]
  async fn can_snapshots_follow_pagination() {
    let api_info = ApiInfo::from_env().unwrap();
    let client = Client::new(api_info);
    let mut request = ListReqInit {
      limit: Some(2),
      ..Default::default()
    }
    .init("AAPL");

    let mut res = client.issue::<List>(&request).await.unwrap();
    let snapshots = res.snapshots;

    assert_eq!(snapshots.len(), 2);
    request.page_token = res.next_page_token;

    res = client.issue::<List>(&request).await.unwrap();
    let new_snapshots = res.snapshots;

    assert_eq!(new_snapshots.len(), 2);
    assert!(res.next_page_token.is_some())
  }
}
