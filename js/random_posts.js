var numberOfPosts = 5;
var minutesBetween = 2;

showRandomPosts(numberOfPosts, function(){});
setInterval(function(){
$('#random-posts').slideUp(800, function(){
    $('#random-posts ul').empty();
    showRandomPosts(numberOfPosts, function(){
        $('#random-posts').slideDown(800);
    });
});
}, minutesBetween*10000);
showBanner();

function showRandomPosts(n, callback)
{
    $.getJSON('/postsList.json', function(data) {
        posts = getRandomPost(data, n);
        for (i = 0; i < posts.length; i++)
        {
            displayPost(posts[i]);
        }
        callback();
    });

}

function displayPost(post)
{
    $('#random-posts ul').append(
        "<a href=\""+post.url+"\"><li>"+post.title+"</li></a>");
}


function getRandomPost(posts, n)
{
    postNumbers = [];
    rndPosts = [];

    while (postNumbers.length < n)
    {
        rnd = random(0, posts.length);
        if ($.inArray(rnd, postNumbers) == -1)
        {
            postNumbers.push(rnd);
        }
    }

    for (i = 0; i < n; i++)
    {
        rndPosts.push(posts[postNumbers[i]]);
    }

    return rndPosts;
}

function random(min, max)
{
    return Math.floor((Math.random() * max) + min);
}

function showBanner()
{
    console.log('\n');
    console.log('  ____  _ _ _               ____        _             ');
    console.log(' / ___|(_) | |_   _        | __ ) _   _| |_ ___  ___  ');
    console.log(' \\___ \\| | | | | | |       |  _ \\| | | | __/ _ \\/ __| ');
    console.log('  ___) | | | | |_| |       | |_) | |_| | ||  __/\\__ \\ ');
    console.log(' |____/|_|_|_|\\__, |       |____/ \\__, |\\__\\___||___/ ');
    console.log('              |___/               |___/               ');
    console.log('\n');
    console.log('By: Daniel Campoverde Carrión');
    console.log('Mailto: alx@sillybytes.net');
    console.log('Creative Commons: Attribution, Share alike');
    console.log('\n');
}
