const transparency    = 'AA';
const scaling_logbase = 2;
const scaling_linear  = 8;
const min_r           = 5;

const color = {


    Aave:          '#B6509E',
    BinanceCoin:   '#F3BB2B',
    Bitcoin:       '#F79413',
    Cardano:       '#0030AF',
    Chainlink:     '#345BD3',
    Cosmos:        '#2E3148',
    CryptocomCoin: '#02154B',
    Dogecoin:      '#BBA030',
    EOS:           '#000000',
    Ethereum:      '#626890',
    Generic:       '#2855AA',
    IOTA:          '#242424',
    Litecoin:      '#315D9E',
    Monero:        '#FF6600',
    NEM:           '#4FBAAD',
    Polkadot:      '#E30475',
    Solana:        '#28ACAA',
    Stellar:       '#000000',
    TRON:          '#FF0013',
    Tether:        '#26A17B',
    USDCoin:       '#3ACC8A',
    Uniswap:       '#EC4297',
    XRP:           '#222222',
};

// Chart.register(ChartDataLabels);

const config = {
    type: 'bubble',
    data: {
        labels: [1, 2],
        datasets: [],
    },
    options: {
        responsive: true,
        layout: {
            padding: 20,
        },
        scales: {
            y: { title: "test", display: true, beginAtZero: true, type: 'logarithmic', min: 0, max: 1000000},
            x: { title: "test", display: true, beginAtZero: true, type: 'logarithmic', min: 0, max: 100000},
        },
        plugins: {
            title: {
                display: true,
                text: "Valor de las Criptomonedas",
                font: {
                    size: 40
                }
            },

            datalabels: {
                anchor: function(context) {
                    var value = context.dataset.data[context.dataIndex];
                    return value.r < 15 ? 'end' : 'center';
                },
                align: function(context) {
                    var value = context.dataset.data[context.dataIndex];
                    return value.r < 15 ? 'end' : 'center';
                },
                color: function(context) {
                    var value = context.dataset.data[context.dataIndex];
                    return value.r < 15 ? context.dataset.backgroundColor : 'white';
                },
                font: {
                    weight: 'bold'
                },
                formatter: function(value) {
                    if (value.r > 0 ) {
                        let usd = Math.round(scaling_logbase ** ((value.r - min_r) / scaling_linear));
                        if (usd > 1) {
                            return "$ " + usd;
                        } else {
                            return "< $ 1";
                        }
                    } else {
                        return "0";
                    }
                },
                offset: 2,
                padding: 0
            }
        }
    }
};

const chart = new Chart(document.getElementById('chart'), config);


function clearChart(chart) {
    chart.data.labels = [];
    chart.data.datasets = [];
}

function insert_coin_dataset(chart, coinName, value, volume, marketcap) {
    let coinDataset = {
        label: coinName,
        data: [{r: value, x: volume, y: marketcap}],
        backgroundColor: color[coinName] + transparency,
        borderWidth: 1,
        clip: {left: false, top: false, right: false, bottom: false},
    };

    chart.data.datasets.push(coinDataset);
}


function insert_or_update_coin_dataset(chart, coinName, value, volume, marketcap) {
    let datasets = chart.data.datasets;
    for (var i in datasets) {
        if (datasets[i].label == coinName) {
            // Update dataset
            console.log("Updating " + coinName);
            datasets[i].data = [{r: value, x: volume, y: marketcap}];
            return;
        }
    }

    console.log("Inserting " + coinName);
    insert_coin_dataset(chart, coinName, value, volume, marketcap);
}

function to_human_month(nString) {
    let n = parseInt(nString);

    switch (n) {
        case 1:  return "Enero";
        case 2:  return "Febrero";
        case 3:  return "Marzo";
        case 4:  return "Abril";
        case 5:  return "Mayo";
        case 6:  return "Junio";
        case 7:  return "Julio";
        case 8:  return "Agosto";
        case 9:  return "Septiembre";
        case 10: return "Octubre";
        case 11: return "Noviembre";
        case 12: return "Diciembre";
    }
}

function logBase(x, base) {
    return Math.log(x) / Math.log(base);
}

function scaleRadious(x) {
    if (x > 1) {
        return (min_r + ((logBase(x, scaling_logbase) * scaling_linear)));
    } else {
        return min_r;
    }
}

// Date: year.month
//       2013.04
function apply_data_for_date(chart, date) {
    newData = monedas[date];
    console.log(newData);

    for (var i in newData) {
        let datum = newData[i];
        insert_or_update_coin_dataset(chart,
            datum['coin'],
            scaleRadious(datum['value']),
            datum['volume'] / 1000000,
            Math.round(datum['marketcap']) / 1000000
        );
    }

    chart.update();

    let split = date.split('.');
    let year  = split[0];
    let month = split[1];
    document.getElementById('year').textContent = year;
    document.getElementById('month').textContent = to_human_month(month);
}


function increment_date(date) {
    let maxYear = 2021;
    let maxMonth = 7;

    let split = date.split('.');
    let year  = parseInt(split[0]);
    let month = parseInt(split[1]);

    var newYear = year;
    var newMonth = month;

    // if (year < maxYear) {
    //     newYear += 1;
    // }

    if (month < 12) {
        newMonth += 1;
    } else {
        newMonth = 1;
        if (year < maxYear) {
            newYear += 1;
        }
    }

    if (newMonth < 10) {
        return newYear + '.0' + newMonth;
    } else {
        return newYear + '.' + newMonth;
    }
}

var currentDate = "2013.04";
apply_data_for_date(chart, currentDate);

function next() {
    currentDate = increment_date(currentDate);
    console.log(currentDate);
    apply_data_for_date(chart, currentDate);
}
