console.log("Author: Daniel Campoverde C.");
console.log("Data sources: \n");
console.log("\t https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory?select=coin_Bitcoin.csv");
console.log("\t https://www.kaggle.com/jessevent/all-crypto-currencies");


new fullpage('#fullpage', {
    autoScrolling: true,
});




// ===============================
// Coins by year
// ===============================

const config_coins_by_year = {
    type: 'bar',
    data: {
        labels: ['2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020', 'Enero 2021', 'Noviembre 2021'],
        datasets: [{
            label: "Número de Criptomonedas",
            data: [7, 67, 501, 572, 636, 1359, 2086, 2403, 4154, 6632],
            backgroundColor: [
                'rgba(54, 162, 235, 1)',
                'rgba(54, 162, 235, 0.2)',
                'rgba(54, 162, 235, 0.3)',
                'rgba(54, 162, 235, 0.4)',
                'rgba(54, 162, 235, 0.5)',
                'rgba(54, 162, 235, 0.6)',
                'rgba(54, 162, 235, 0.7)',
                'rgba(54, 162, 235, 0.8)',
                'rgba(54, 162, 235, 0.9)',
                'rgba(54, 162, 235, 1)',
            ],
        }]
    },
    options: {
        responsive: true,
        scales: {
            y: {
                beginAtZero: true
            }
        }
    }
};

const chart_coins_by_year = new Chart(document.getElementById('chart-coins-by-year'), config_coins_by_year);





// ===============================
// Users by continent
// ===============================

const config_users_by_continent = {
    type: 'bar',
    data: {
        labels: ['Oceania', 'Sudamérica', 'Norteamérica', 'África', 'Europa', 'Asia'],
        datasets: [{
            label: "Número de Usuarios (millones)",
            data: [1, 24, 28, 32, 38, 160],
            backgroundColor: [
                'rgba(75, 192, 192, 0.2)',
                'rgba(75, 192, 192, 0.4)',
                'rgba(75, 192, 192, 0.6)',
                'rgba(75, 192, 192, 0.8)',
                'rgba(75, 192, 192, 0.9)',
                'rgba(75, 192, 192, 1)',
            ],
        }]
    },
    options: {
        responsive: true,
        indexAxis: 'y',
        scales: {
            y: {
                beginAtZero: true
            }
        }
    }
};

const chart_users_by_continent = new Chart(document.getElementById('chart_users_by_continent'), config_users_by_continent);





// ===============================
// Algoritmos
// ===============================

const config_algoritmos = {
    type: 'polarArea',
    data: {
        labels: ['Scrypt', 'SHA256', 'Ethash', 'CyptoNight', 'Otros'],
        datasets: [{
            label: "Algoritmos de hashing populares",
            data: [84, 49, 34, 32, 44],
            backgroundColor: [
                'rgba(54, 162, 235, 0.7)',
                'rgba(255, 159, 64, 0.7)',
                'rgba(255, 205, 86, 0.7)',
                'rgba(75, 192, 192, 0.7)',
                'rgba(201, 203, 207, 0.7)',
            ],
        }]
    },
};

const chart_algoritmos = new Chart(document.getElementById('chart_algoritmos'), config_algoritmos);





// ===============================
// Energy
// ===============================

const config_energy = {
    type: 'bar',
    data: {
        labels: ['Prueba de cobertura (WiFi)', 'Prueba de espacio (HDDs, SSDs)', 'Prueba de Participación (CPU)',
            'Prueba de Trabajo (CPU)', 'Prueba de Trabajo (GPU)', 'Prueba de Trabajo (ASIC)'
        ],
        datasets: [{
            label: "Energía promedio por nodo de minado",
            data: [10, 14, 55, 230, 290, 850],
            backgroundColor: [
                'rgba(54, 162, 235, 0.8)',
                'rgba(75, 192, 192, 0.5)',
                'rgba(75, 192, 192, 0.8)',
                'rgba(255, 205, 86, 0.8)',
                'rgba(255, 159, 64, 0.8)',
                'rgba(255, 99, 132, 0.8)',
            ],
        }]
    },
    options: {
        layout: {
            padding: 20,
        },
        responsive: true,
        scales: {
            y: {
                beginAtZero: true
            }
        },
        plugins: {
            deferred: {
                xOffset: 150,   // defer until 150px of the canvas width are inside the viewport
                yOffset: '50%', // defer until 50% of the canvas height are inside the viewport
                delay: 500      // delay of 500 ms after the canvas is considered inside the viewport
            }
        },
    }
};

const chart_energy = new Chart(document.getElementById('chart_energy'), config_energy);





// ===============================
// Comparacion
// ===============================

const config_comparacion = {
    type: 'radar',
    data: {
        labels: ['Valor relativo(log)', 'Energia requerida por nodo', 'Indice de privacidad relativa',
            'Indice de popularidad', 'Costo de hardware de minado por nodo (log)'
        ],
        datasets: [
            {
                label: "Bitcoin",
                data: [100, 100, 20, 90, 100],
                fill: true,
                backgroundColor:           '#F7941322',
                borderColor:               '#F79413',
                pointHoverBorderColor:     '#F79413',
                pointBackgroundColor:      '#F79413',
                pointBorderColor:          '#FFF',
                pointHoverBackgroundColor: '#FFF',
            },

            {
                label: "Monero",
                data: [15, 40, 100, 20, 20],
                fill: true,
                backgroundColor:           '#315D9E22',
                borderColor:               '#315D9E',
                pointHoverBorderColor:     '#315D9E',
                pointBackgroundColor:      '#315D9E',
                pointBorderColor:          '#FFF',
                pointHoverBackgroundColor: '#FFF',
            },

            {
                label: "Chia",
                data: [10, 10, 20, 10, 10],
                fill: true,
                backgroundColor:           '#6ec0a722',
                borderColor:               '#6ec0a7',
                pointHoverBorderColor:     '#6ec0a7',
                pointBackgroundColor:      '#6ec0a7',
                pointBorderColor:          '#FFF',
                pointHoverBackgroundColor: '#FFF',
            },

        ]
    },
    options: {
        scales: {
            r: {
                angleLines: {
                    display: false
                },
                suggestedMin: 1,
                suggestedMax: 100
            }
        },
        elements: {
            line: {
                borderWidth: 3
            }
        }
    }
};

const chart_comparacion = new Chart(document.getElementById('chart_comparacion'), config_comparacion);
