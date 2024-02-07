something = getLuaStore()
drawText(something, 400, 500)
drawText(getTicks(), 100, 200)
local x, y = getMousePosition()
mouseText = "Mouse X:" .. x .. "Mouse Y:" .. y
drawText(mouseText, 300, 400)