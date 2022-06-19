var ul = document.getElementById('cortisonMeasurements');

function generateRandomInteger(min, max) {
  return Math.floor(min + Math.random()*(max - min + 1))
}

function insertMeasurement()
{
    var li = document.createElement('li');
    li.className = 'standout';
    li.appendChild(document.createTextNode(generateRandomInteger(10, 15) + '  μg/dL'));
    ul.appendChild(li);
}

setInterval(insertMeasurement, 2000);
