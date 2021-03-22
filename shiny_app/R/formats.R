

#  ------------------------------------------------------------------------
#
# Title : formats
#    By : Jimmy Briggs
#  Date : 2021-03-22
#
#  ------------------------------------------------------------------------

formats <- list(
  dollars = list(
    tooltip = list(valueDecimals = 0, valuePrefix = '$ '),
    format = "$ {value:,.0f}",
    pointFormat = '{point.y:,.0f}',
    formatter = function(x) {
      x = format(
        x,
        format = "d",
        big.mark = ",",
        digits = 0,
        scientific = FALSE
      )
      x = paste('$ ', stringr::str_pad(x, width = max(nchar(x)), side = 'left'))
      return(x)
    }
  ),

  percentage = list(
    tooltip = list(valueDecimals = 1, valueSuffix = '%'),
    format = "{value}%",
    pointFormat = '{point.y:.0f}%',
    formatter = function(x)
      paste0(round(x * 100, 0), '%')
  ),

  decimal = list(
    tooltip = list(valueDecimals = 3),
    format = "{value:.3f}",
    pointFormat = '{point.y:.3f}',
    formatter = function(x) {
      out = format(x, digits = 3, scientific = FALSE)
      out[which(x == 0)] <- 0
      return(out)
    }
  ),

  count = list(
    tooltip = list(valueDecimals = 0),
    format = "{value:,.0f}",
    pointFormat = '{point.y:,.0f}',
    formatter = function(x)
      gsub('[$]', '', scales::dollar(round(x, 0)))
  ),

  date = list(
    formatter = function(x)
      gsub('^0', '', format(x, '%m/%d/%Y'))
  )
)
