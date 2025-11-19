trades <<- NULL

# Setup function, called once before the backtest starts.
# Parameters are inherited from backtest() call.
#
before <- function(...) {
  params <- list(...)
  trades <<- cbind(params$trades, order_id = NA)
  colnames(trades) <- c(colnames(params$trades), "order_id")
}

after <- function() {
  if (total_positions(status = CLOSED) != nrow(trades)) {
    print("Not all trades were executed")
  }
}

# Context:
# - Bars: global iteration step `I`
#
# Global functions:
# - balance
# - positions
# - total_positions
# - buy, sell: send order
# - ask, bid: multi-instrument
#
tick <- function() {
  # Find trades that should be entered: entry_date has passed and no order_id yet
  trade_ids <- which(
    !is.na(trades$entry_date) & 
      is.na(trades$order_id) & 
      Date() >= trades$entry_date
  )
  
  if (length(trade_ids) > 0) {
    for (trade_id in trade_ids) {
      ticker_id <- select_ticker_name(trades$symbol[trade_id])
      if (trades$allocation[trade_id] > 0) {
        position_size <- floor((trades$allocation[trade_id] * balance()) / ask(trades$symbol[trade_id]))
        order_id <- buy(
          position_size,
          ticker = ticker_id
        )
        trades$order_id[trade_id] <<- order_id
      } else if (trades$allocation[trade_id] < 0) {
        position_size <- floor((-trades$allocation[trade_id] * balance()) / bid(trades$symbol[trade_id]))
        order_id <- sell(
          position_size,
          ticker = ticker_id
        )
        trades$order_id[trade_id] <<- order_id
      } else {
        stop("Allocation cannot be zero")
      }
    }
  }
}

# Position Management.
# This function is called before every tick for each open position.
#
# Available:
# - BARS: position age in bars
# - Bars: iteration step, global `I`
# - S: last price / entry price, floats around 1.0
# - Ask, Bid: current ask and bid prices for this instrument
# - current_price: latest price of the instrument, should be either Ask/Bid
#
# Position attributes:
# - id: unique identifier of the position
# - pos_type: LONG or SHORT
# - status: OPEN
# - qty: lot size
# - entry price, entry time: price and time of position opening
# - take profit, stop loss: price levels for automatic closing
# - order id: unique identifier of the order that opened the position
#
# Position functions:
# - close_position()
# - close_partial_position(qty): must be less than position's `qty` (lot size)
# - update_take_profit(new_price)
# - update_stop_loss(new_price)
#
position <- function() {
  exit_date <- trades$exit_date[which(trades$order_id == order_id)]
  if (length(exit_date) > 0 && Date() >= exit_date[1]) {
    close_position()
  }
}
