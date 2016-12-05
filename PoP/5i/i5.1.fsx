let concat xss = List.foldBack (@) xss []

concat [[2];[6; 4]; [1]]
