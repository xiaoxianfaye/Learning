-module(draw).
-compile(export_all).

drawline(StartP, EndP) ->
    Image = egd:create(200, 200),
    Blue = egd:color({0, 0, 255}),
    egd:line(Image, StartP, EndP, Blue),
    egd:save(egd:render(Image, png), "./test1.png"),
    egd:destroy(Image).