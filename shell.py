import MAI
while True:
    text = input('MAI > ')
    result, error = MAI.run('<stdin>',text)

    if error:
        print(error.as_string())
    else:
        print(result)   