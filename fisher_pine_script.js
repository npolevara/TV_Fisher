//@version=3
// author Octacat
// strategy(title="Fisher 0.1", shorttitle="Fisher 0.1", overlay=true, precision=6, pyramiding=0, initial_capital=100000, currency="USD", default_qty_type=strategy.percent_of_equity,calc_on_order_fills= false, calc_on_every_tick=false, default_qty_value=99.0, commission_type=strategy.commission.percent, commission_value=0.05)

study(title="Fisher signal 0.1", shorttitle="Fisher signal 0.1", overlay=true)

oker        = #FF9900   , reds          = #FF6600   , dreds         = #5d2b0a   ,
cyan        = #00CCCC   , blues         = #0099CC   , dblues        = #0a3f51   , 
magenta     = #FF00FF   , purples       = #6633CC   , KBBB          = #BBBBBB   , 
K111        = #111111   , K555          = #555555   , K999          = #999999   , 

//--------------spining -time
t_m = input(10,'Min',minval=0,maxval=59,step=1)
t_h = input(14,'Hours',minval=0,maxval=23,step=1)
t_d = input(06,'Days',minval=1,maxval=31,step=1)
t_mo = input(07,'Month',minval=01,maxval=12,step=1)
t_y = input(2018,'Year',minval=2018,maxval=2020)
gmt = input("GMT+3","Timezone")

t1 = timestamp(gmt,t_y, t_mo, t_d, t_h, t_m)
point = crossunder(t1,time)
// -------------spining -end
plotshape( point, color=oker, style=shape.triangledown, location=location.top, size=size.tiny )
// plotshape( t2, color=cyan, style=shape.triangledown, location=location.top, size=size.tiny )
// plotshape( t3, color=magenta, style=shape.triangledown, location=location.top, size=size.tiny )
// plotshape( t1, color=oker, style=shape.triangledown, location=location.top, size=size.tiny )

count = 0.0

rb = close < open
gb = close > open
rb2 = rb and rb[1]
gb2 = gb and gb[1]

ema_fun(src,speed) =>
    emaax = ema(src,speed)
    emaax

dtp(a,div) =>
    result = (a*a)/div
    result   

deep_trailing(entry,usage,percent,deep_window,decr,input_a,use_ema,ema_src,ema_size) =>
    src_trail_SD = use_ema ? ema_fun(ema_src,ema_size):close

    decrease_SD = 0.0
    signal_SD = 0
    signal_SD := nz(signal_SD[1])

    BuySig_for_SD = usage?entry:na

    last_BuySig_for_SD = na
    last_BuySig_for_SD := BuySig_for_SD ? close : nz(last_BuySig_for_SD[1])
    total_BuySig_for_SD = 0.0
    total_BuySig_for_SD := nz(total_BuySig_for_SD[1])



    if BuySig_for_SD
        signal_SD := signal_SD + 1
        total_BuySig_for_SD := total_BuySig_for_SD + last_BuySig_for_SD

    av_deep_SD = 0.0
    av_deep_SD := nz(av_deep_SD[1])
    av_deep_SD := total_BuySig_for_SD / signal_SD

    deep_variat_SD = av_deep_SD - av_deep_SD / 100 * percent

    deep_SD = na
    deepPlot_SD = na
    deep_SD := input_a < deep_variat_SD ? 1 : 0
    deepPlot_SD := deep_variat_SD

    deep_buy_SD = na
    lastLow_SD = 0.0
    lastLow_SD := nz(lastLow_SD[1])
    tr_DeepStop_SD = 0.0
    tr_DeepStop_SD := nz(tr_DeepStop_SD[1])

    if deep_SD or input_a < lastLow_SD[1] or (deep_SD and input_a < lastLow_SD[1])
        decrease_SD := nz(decrease_SD[1]) + decr
        lastLow_SD := input_a
        tr_DeepStop_SD := lastLow_SD + av_deep_SD / 100 * (deep_window - decrease_SD)

    deep_ok_cond_SD = tr_DeepStop_SD > 0



    if deep_ok_cond_SD
        deep_buy_SD := src_trail_SD > tr_DeepStop_SD



    if deep_buy_SD
        // bet_signal := 0
        decrease_SD := 0.0
        av_deep_SD := 0
        total_BuySig_for_SD := 0.0
        signal_SD := 0
        tr_DeepStop_SD := 0.0
        lastLow_SD := 0.0


    // plot(tr_DeepStop_SD > 0 ? tr_DeepStop_SD : na, color=yellow, style=circles, linewidth=1)
    // plot(lastLow_SD > 0 ? lastLow_SD : na, color=cyan, style=circles, linewidth=1)
    // plotshape( deep_buy_SD, color=dreds, style=shape.triangleup, location=location.bottom, size=size.small )

    result = deep_buy_SD

//----------------end - deep module---


//------------railing-exit module-----
trail_module(entry,activate,trail,decr,prof,downgrade,parabola,ema,ema_src,ema_length,parabola_counter) =>

    src_act_tr_ema = ema?ema_fun(ema_src,ema_length):close

    koff = downgrade?dtp(parabola_counter,parabola):0.0

// if res_filter_pro
//     closePercentProfit := input(0.5, "Activate Trailing after dwntrend filter",minval=0.1,maxval=25,step=0.1)
//     closePercentProfit := closePercentProfit - koff
    decrease_tr = 0.0

    signalSection = 0
    signalSection := nz(signalSection[1])

    buy = entry



    lastClose = na
    lastClose := buy ? close : nz(lastClose[1])
    total = 0.0
    total := nz(total[1])



    if buy
        // buy_on := 1
        signalSection := signalSection + 1
        total := total + lastClose

    average = 0.0
    average := nz(average[1])
    average := total / signalSection


    profit = na
    profitPlot = na
    pro_cond = average + average / 100 * activate

    profit := prof > pro_cond  ? 1 : 0
    profitPlot := pro_cond


    sell = na
    lastHigh = 0.0
    lastHigh := nz(lastHigh[1])
    trailingStop = 0.0
    trailingStop := nz(trailingStop[1])

    if profit and src_act_tr_ema > lastHigh[1]
        decrease_tr := decrease_tr == 0.0?nz(decrease_tr[1]) + decr: decrease_tr + decr
        lastHigh := src_act_tr_ema
        trailingStop := lastHigh - average / 100 * (trail - decrease_tr)
    
    sell := src_act_tr_ema < trailingStop

    if sell
        decrease_tr := 0.0
        // buy_on := 0
        average := 0
        total := 0.0
        signalSection := 0
        trailingStop := 0.0
        lastHigh := 0.0

    result = sell
    pro_plot = profitPlot > close ? profitPlot : na
    tr_stop = trailingStop > 0 ? trailingStop : na
    lh = lastHigh > 0 ? lastHigh : na

		// plotshape( sell, color=lgren, style=shape.triangleup, location=location.bottom, size=size.tiny )
		// plotshape( close_long1, color=rrr, style=shape.triangledown, location=location.top, size=size.tiny )

    [sell,pro_plot,tr_stop,lh]





//------------end trailing-exit-----
// second_deep_buy_SD = true

// percentdeep_SD= input(-0.1, "Activate deep Trailing",step=0.1)
// trail_DV = input(0.3, "Trailing deep Percent",step=0.1)
// decr_sec_deep= input(0.01, "Decrease deep Trailing Percent",minval=0.000,maxval=1,step=0.002)
// sec_deep_src = input(low,'deep trailing src')

// use_ema_or_close_for_SDV = false
// sec_deep_src2 = close
// adsp_sec= 4


// use_exit_trail = input(true,"Use exit trailing")
closePercentProfit = input(1.5, "Activate exit trailing",maxval=25,step=0.1)
trailingStopValue = input(0.5, "Trailing Percent",minval=0.1,maxval=25,step=0.1)
decr_tr = input(0.045, "Decrease Trailing Percent",minval=0.0,maxval=1,step=0.01)

prof_close = input(close,'Trailing cross')
use_float_tp = false
// use_float_tp = input(false,"Downgrade Trailing Activation")
dtp_p = 80
// dtp_p = input(80,"DTA Parabola")

use_ema_or_close_at = false
tra_src2 = close
asp = 4


[sell_t,prof_plot,trs,lasth] = trail_module(point,closePercentProfit,trailingStopValue,decr_tr,prof_close,use_float_tp,dtp_p,use_ema_or_close_at,tra_src2,asp,count)

buy = point
sell = sell_t


plot(prof_plot, "Profit", color=lime, linewidth=2, style=linebr)
plot(trs, color=yellow, style=linebr, linewidth=2)
plot(lasth, color=white, style=linebr, linewidth=2)

plotshape( sell_t, color=lime, style=shape.triangledown, location=location.top, size=size.tiny )

alertcondition( sell_t, 'Sell point', 'Time to make some profit' )


// testStartYear = input(2018, "From Year") 
// testStartMonth = input(1, "From Month")
// testStartDay = input(1, "From Day")
// testPeriodStart = timestamp(testStartYear,testStartMonth,testStartDay,0,0)

// testStopYear = input(2019, "To Year")
// testStopMonth = input(1, "To Month")
// testStopDay = input(1, "To Day")
// testPeriodStop = timestamp(testStopYear,testStopMonth,testStopDay,0,0)

// testPeriod() =>
//     time >= testPeriodStart and time <= testPeriodStop ? true : false


// if testPeriod()
//     if buy
//         strategy.entry(".", true)
    

//     if sell
//         strategy.close(".")
   
//  