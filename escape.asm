.8086
.model small
.stack 100h
.data
    char_to_print db 01h, 24h
    salto db 0dh, 0ah, 24h


    cartelInicio db 0dh,0ah, '          :::::::::: ::::::::   ::::::::      :::     :::::::::  :::::::::: ', 0dh, 0ah
                 db          '         :+:       :+:    :+: :+:    :+:   :+: :+:   :+:    :+: :+:         ', 0dh, 0ah     
                 db          '        +:+       +:+        +:+         +:+   +:+  +:+    +:+ +:+          ', 0dh, 0ah     
                 db          '       +#++:++#  +#++:++#++ +#+        +#++:++#++: +#++:++#+  +#++:++#      ', 0dh, 0ah     
                 db          '      +#+              +#+ +#+        +#+     +#+ +#+        +#+            ', 0dh, 0ah     
                 db          '     #+#       #+#    #+# #+#    #+# #+#     #+# #+#        #+#             ', 0dh, 0ah     
                 db          '    ########## ########   ########  ###     ### ###        ##########       ', 0dh, 0ah
             db 0dh,0ah,0dh,0ah,"          PRESIONE ENTER PARA COMENZAR A JUGAR!      ",  24h
    
    tutorial db '     ___________________________________________________ ', 0dh, 0ah
             db '    |                                                   |', 0dh, 0ah
             db '    |   Instrucciones:             Jugador: ', 01h, '           |', 0dh, 0ah
             db '    |                                                   |', 0dh, 0ah
             db '    |     Moverse:                 Enemigo: ', 02h, '           |', 0dh, 0ah
             db '    |           +-----+                                 |', 0dh, 0ah
             db '    |           |  ^  |              Escaparse:         |', 0dh, 0ah
             db '    |     +-----+-----+-----+                           |', 0dh, 0ah
             db '    |     |  <  |  v  |  >  |                 |    |    |', 0dh, 0ah
             db '    |     +-----+-----+-----+                 |----|    |', 0dh, 0ah
             db '    |                                         |    |    |', 0dh, 0ah
             db '    |     Atacar:   +---------------+                   |', 0dh, 0ah
             db '    |               |    Espacio    |                   |', 0dh, 0ah
             db '    |               +---------------+                   |', 0dh, 0ah
             db '    |___________________________________________________|', 0dh, 0ah
             db 0dh,0ah, '          PRESIONE ENTER PARA COMENZAR A JUGAR!      ', 24h

    perdiste db 0dh,0ah, '      ::::::::      :::     ::::    ::::  ::::::::::     ', 0dh, 0ah
             db          '     :+:    :+:   :+: :+:   +:+:+: :+:+:+ :+:            ', 0dh, 0ah
             db          '     +:+         +:+   +:+  +:+ +:+:+ +:+ +:+            ', 0dh, 0ah
             db          '     :#:        +#++:++#++: +#+  +:+  +#+ +#++:++#       ', 0dh, 0ah
             db          '     +#+   +#+# +#+     +#+ +#+       +#+ +#+            ', 0dh, 0ah
             db          '     #+#    #+# #+#     #+# #+#       #+# #+#            ', 0dh, 0ah
             db          '      ########  ###     ### ###       ### ##########     ', 0dh, 0ah,0dh,0ah
             db          '       ::::::::  :::     ::: :::::::::: :::::::::        ', 0dh, 0ah
             db          '      :+:    :+: :+:     :+: :+:        :+:    :+:       ', 0dh, 0ah
             db          '      +:+    +:+ +:+     +:+ +:+        +:+    +:+       ', 0dh, 0ah
             db          '      +#+    +:+ +#+     +:+ +#++:++#   +#++:++#:        ', 0dh, 0ah
             db          '      +#+    +#+  +#+   +#+  +#+        +#+    +#+       ', 0dh, 0ah
             db          '      #+#    #+#   #+#+#+#   #+#        #+#    #+#       ', 0dh, 0ah
             db          '       ########      ###     ########## ###    ###       ', 0dh, 0ah
               db 0dh,0ah,0dh,0ah,"           PRESIONE CUALQUER TECLA PARA SALIR           ", 24h

    ganaste db 0dh, 0ah, '             :::   :::  ::::::::  :::    :::             ', 0dh, 0ah 
            db           '             :+:   :+: :+:    :+: :+:    :+:             ', 0dh, 0ah
            db           '              +:+ +:+  +:+    +:+ +:+    +:+             ', 0dh, 0ah
            db           '               +#++:   +#+    +:+ +#+    +:+             ', 0dh, 0ah
            db           '                +#+    +#+    +#+ +#+    +#+             ', 0dh, 0ah
            db           '                #+#    #+#    #+# #+#    #+#             ', 0dh, 0ah
            db           '                ###     ########   ########              ', 0dh, 0ah, 0dh, 0ah
            db           '          :::       ::: ::::::::::: ::::    :::          ', 0dh, 0ah
            db           '          :+:       :+:     :+:     :+:+:   :+:          ', 0dh, 0ah
            db           '          +:+       +:+     +:+     :+:+:+  +:+          ', 0dh, 0ah
            db           '          +#+  +:+  +#+     +#+     +#+ +:+ +#+          ', 0dh, 0ah
            db           '          +#+ +#+#+ +#+     +#+     +#+  +#+#+#          ', 0dh, 0ah
            db           '           #+#+# #+#+#      #+#     #+#   #+#+#          ', 0dh, 0ah
            db           '            ###   ###   ########### ###    ####          ', 0dh, 0ah
            db "        Mas mapas seran agregados en un futuro.         ", 0dh, 0ah
            db 0dh,0ah,"           PRESIONE ENTER PARA CONTINUAR               ", 24h

    

 mapa db '+--------------------------------------------------------------------+', 0dh, 0ah
      db '|####################################################################|', 0dh, 0ah
      db '|####################################################################|', 0dh, 0ah
      db '|############+---------------------------------------------------+###|', 0dh, 0ah
      db '|############|                                                   |###|', 0dh, 0ah
      db '|############|                  +---------------+        +--+    |###|', 0dh, 0ah
      db '|############|                  |###############|        |##|    |###|', 0dh, 0ah
      db '|############+------+     +-----+###############+--------+##|    |###|', 0dh, 0ah
      db '|###################|     |#################################|    |###|', 0dh, 0ah
      db '|###################|     |#################################|    |###|', 0dh, 0ah
      db '|###################|     |#################################|    |###|', 0dh, 0ah
      db '|############+------+     +----------------------+##########|    |###|', 0dh, 0ah
      db '|############|                                   |##########|    |###|', 0dh, 0ah
      db '|############|                        H          |##########|    |###|', 0dh, 0ah
      db '|############|                                   |##########|    |###|', 0dh, 0ah
      db '|############+-----------------------------------+##########|    |###|', 0dh, 0ah
      db '|###########################################################|    |###|', 0dh, 0ah
      db '|###########################################################|    |###|', 0dh, 0ah
      db '|############+-----------------------------------+##########|    |###|', 0dh, 0ah
      db '|############|                                   |##########|    |###|', 0dh, 0ah
      db '|############|                                   +----------+    |###|', 0dh, 0ah
      db '|############|                                                   |###|', 0dh, 0ah
      db '|############+-----------------------------------+---------------+###|', 0dh, 0ah
      db '|####################################################################|', 0dh, 0ah
      db '+--------------------------------------------------------------------+', 24h
    posX db 24          ; Posición X inicial del personaje (Centro de la habitación A)
    posY db 20          ; Posición Y inicial del personaje (Centro de la habitación A)
    jugador db 24, 20, 10, 2       ;pox x, pos y, vida, ataque
    limiteIzquierda db 0   ; Límite izquierdo del mapa
    limiteDerecha db 69    ; Límite derecho del mapa
    limiteArriba db 0      ; Límite superior del mapa
    limiteAbajo db 24     ; Límite inferior del mapa


    charEnemigo db 02h
    enemigo db 00h, 24h, 0dh, 10, 1 ; id, posX, posY, vida, ataque
    mensaje db 'Presiona cualquier tecla para continuar...', '$'
    last_tick dw 0

.code
;public map
main proc
    mov ax, @data
    mov ds, ax
  
inicio: 
; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h
    ; Imprimir cada parte del cartel por separado
    mov dx, offset cartelInicio
    mov ah, 09h
    int 21h

    mov ah, 00h
    int 16h
    cmp al, 0dh
    je tuto 
    jmp inicio

tuto: 
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h
    ; Imprimir cada parte del cartel por separado
    mov dx, offset tutorial
    mov ah, 09h
    int 21h

    mov ah, 00h
    int 16h
    cmp al, 0dh
    je comienzo
    jmp tuto

comienzo:    ; Obtener el tick inicial
    call GetTickCount
    mov [last_tick], dx

print_rows:
call WaitForRetrace
call print_rs

cmp byte ptr [jugador + 2], 0
jne noMuere
jmp gameOver
noMuere:

    mov ah, 01h
    int 16h
    jz no_key_pressed  ; Si no hay tecla presionada, salta a 'no_key_pressed'

    ; Leer la tecla presionada
    mov ah, 00h
    int 16h

    ; Comprobar si se ha presionado Escape (ASCII 1Bh)
    cmp al, 1Bh         ; Compara con la tecla Escape
    jne noSalir
    jmp exit_game        ; Si se presionó Escape, salta a la etiqueta "exit_game"

no_key_pressed:


noSalir:
    ; Movimiento del personaje
    cmp ah, 48h         ; Flecha arriba
    jne noMovAr
    jmp moveUp
noMovAr:
    cmp ah, 4Bh         ; Flecha izquierda
    jne noMovIz
    je moveLeft
noMovIz:
    cmp ah, 4Dh         ; Flecha derecha
    jne noMovDe
    jmp moveRight
noMovDe:
    cmp ah, 50h         ; Flecha abajo
    jne noMovAb
    jmp moveDown
noMovAb:
    cmp al, 20h
    jne noAtackPlayer
    call atackPlayer
noAtackPlayer:

    jmp print_rows     ; Volver a imprimir el mapa si no se presionó una tecla de movimiento

moveUp:
    ; Verificar si la próxima posición está ocupada por un guion
    xor ax, ax
    xor bx, bx
    mov bl, [jugador]
    mov al, [jugador + 1]       ; Copia la posición Y a AX

    call choque_arriba

    mov ah, offset mapa[bx]
    call paredes
    cmp cx, 1
    jne seguir
    jmp redraw
    seguir:

    cmp mapa[bx], 'H'
    jne seguir2
    jmp win
    seguir2:
    ; Cargar el límite superior en un registro antes de la comparación
    ; mov al, limiteArriba
    ; cmp posY, al
    ; jg seguir3
    ; jmp redraw         ; Si el personaje está en el límite superior, no lo muevas hacia arriba
    ; seguir3:
    dec [jugador + 1]           ; Mueve hacia arriba ;;;;;; SOLO ESTO HACE EL MOV. REAL
    jmp redraw

moveLeft:
    xor ax, ax
    xor bx, bx
    mov bl, [jugador]
    mov al, [jugador + 1]       ; Copia la posición Y a AX

    call choque_izquierda

    mov ah, offset mapa[bx]
    call paredes
    cmp cx, 1
    jne seguir3
    jmp redraw
    seguir3:    

    cmp mapa[bx], 'H'
    jne seguir4
    jmp win
    seguir4:

    mov al, limiteIzquierda
    cmp [jugador], al
    jle redraw        ; Si el personaje está en el límite izquierdo, no lo muevas hacia la izquierda
    dec [jugador]          ; Mueve hacia la izquierda
    jmp redraw

moveRight:
    xor ax, ax
    xor bx, bx
    mov bl, [jugador]
    mov al, [jugador + 1]       ; Copia la posición Y a AX
    ;mov cx, offset mapa

    call choque_derecha
    
    mov ah, offset mapa[bx]
    call paredes
    cmp cx, 1
    jne seguir5
    jmp redraw
    seguir5:    

    cmp mapa[bx], 'H'
    jne seguir6
    jmp win
    seguir6:

    mov al, limiteDerecha
    cmp [jugador], al
    jge redraw        ; Si el personaje está en el límite derecho, no lo muevas hacia la derecha
    inc [jugador]          ; Mueve hacia la derecha
    jmp redraw

moveDown:
; Verificar si la próxima posición está ocupada por un guion
    xor ax, ax
    xor bx, bx
    mov bl, [jugador]
    mov al, [jugador + 1]       ; Copia la posición Y a AX
    ;mov cx, offset mapa

    call choque_abajo
    
    mov ah, offset mapa[bx]
    call paredes
    cmp cx, 1
    jne seguir7
    jmp redraw
    seguir7:    

    cmp mapa[bx], 'H'
    jne seguir8
    jmp win
    seguir8:

    mov al, limiteAbajo
    cmp [jugador + 1], al
    jge redraw        ; Si el personaje está en el límite inferior, no lo muevas hacia abajo
    inc [jugador + 1]          ; Mueve hacia abajo
    jmp redraw

redraw:
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h             ; Llama a la interrupción de video BIOS

    ; Imprimir el mapa
    mov dx, offset mapa ; Guardamos la dirección base del mapa en DX
    mov ah, 09h         ; Función DOS para imprimir una cadena ($-terminated)
    int 21h             ; Llamamos a la interrupción del sistema DOS para imprimir la fila actual

call print_rs

    jmp print_rows      ; Volver a imprimir el mapa después de redibujar el personaje

gameOver:
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h

    mov ah, 02h         ; Posicionar el cursor
    mov bh, 0           ; Página de pantalla
    mov dh, 0   ; Fila
    mov dl, 0  ; Columna
    int 10h             ; Llama a la interrupción de video BIOS

    mov dx, offset perdiste ; Guardamos la dirección base del mapa en DX
    mov ah, 09h         ; Función DOS para imprimir una cadena ($-terminated)
    int 21h

    mov ah, 1
    int 21h
    je exit_game 

win:
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h

    mov ah, 02h         ; Posicionar el cursor
    mov bh, 0           ; Página de pantalla
    mov dh, 0   ; Fila
    mov dl, 0  ; Columna
    int 10h             ; Llama a la interrupción de video BIOS

    mov dx, offset ganaste ; Guardamos la dirección base del mapa en DX
    mov ah, 09h         ; Función DOS para imprimir una cadena ($-terminated)
    int 21h

    mov ah, 1
    int 21h
    je exit_game 


exit_game:
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h             ; Llama a la interrupción de video BIOS

    mov ax, 4c00h       ; Código de salida del programa
    int 21h             ; Llamamos a la interrupción del sistema DOS para salir del programa

main endp

proc WaitForRetrace
    mov dx, 03DAh        ; Cargar la dirección del puerto en DX

    ; Esperar hasta que el retrace vertical comience
WaitForRetraceStart:
    in al, dx            ; Leer el estado del CRT
    test al, 08h         ; Comprobar el bit 3 (vértice de retorno vertical)
    jnz WaitForRetraceStart  ; Si está en el retrace, esperar

    ; Esperar hasta que el retrace vertical termine
WaitForRetraceEnd:
    in al, dx            ; Leer el estado del CRT
    test al, 08h         ; Comprobar el bit 3 (vértice de retorno vertical)
    jz WaitForRetraceEnd ; Si no está en el retrace, esperar

    ret
endp

; Rutina para obtener el contador de ticks del sistema
proc GetTickCount
    mov ah, 00h          ; Función BIOS para leer el contador de tiempo
    int 1Ah              ; Llamada a la interrupción del BIOS
    ret
endp


proc choque_arriba
    dec al
    mov dl, 72
    mul dl
    mov bh, 0 
    add ax, bx
    mov bx, ax
    ret
endp

proc choque_abajo
    inc al
    mov dl, 72
    mul dl
    mov bh, 0 
    add ax, bx
    mov bx, ax
    ret
endp

proc choque_izquierda
    dec bl
    mov dl, 72
    mul dl
    mov bh, 0 
    add ax, bx
    mov bx, ax
    ret
endp

proc choque_derecha
    inc bl
    mov dl, 72
    mul dl
    mov bh, 0 
    add ax, bx
    mov bx, ax
    ret
endp


print_rs proc
    ; Limpiar la pantalla
    mov ah, 0           ; Función para limpiar la pantalla
    mov al, 3           ; Valor para borrar toda la pantalla
    int 10h             ; Llama a la interrupción de video BIOS

    ; Imprimir el mapa
    mov dx, offset mapa ; Guardamos la dirección base del mapa en DX
    mov ah, 09h         ; Función DOS para imprimir una cadena ($-terminated)
    int 21h             ; Llamamos a la interrupción del sistema DOS para imprimir la fila actual

    ; Imprimir el personaje
    mov ah, 02h         ; Posicionar el cursor
    mov bh, 0           ; Página de pantalla
    mov dh, byte ptr [jugador + 1]   ; Fila
    mov dl, byte ptr [jugador]   ; Columna
    int 10h             ; Llama a la interrupción de video BIOS

    ; Imprimir el carácter en la posición especificada
    mov ah, 0Ah      ; Función 0Ah de la interrupción 10h: Imprimir carácter en la posición del cursor
    mov al, char_to_print  ; Cargar el carácter a imprimir en AL
    mov bh, 0        ; Página de visualización (normalmente es 0)
    mov cx, 1        ; Número de veces que se imprimirá el carácter (en este caso, solo una vez)
    int 10h 



    cmp byte ptr [enemigo + 3], 0
    jne ksy
    ; Imprimir el enemigo
    mov ah, 02h           ; Posicionar el cursor para el enemigo
    mov bh, 0             ; Página de pantalla
    mov dh, byte ptr [enemigo + 2]   ; Fila del enemigo
    mov dl, byte ptr [enemigo + 1]   ; Columna del enemigo
    int 10h               ; Llama a la interrupción de video BIOS

    ; Imprimir el carácter del enemigo en la posición especificada
    mov ah, 0Ah           ; Función 0Ah de la interrupción 10h: Imprimir carácter en la posición del cursor
    mov al, '' ; Cargar el carácter del enemigo en AL
    mov bh, 0             ; Página de visualización (normalmente es 0)
    mov cx, 1             ; Número de veces que se imprimirá el carácter (en este caso, solo una vez)
    int 10h 

    jmp wait_loop


ksy:

    ; Mover el enemigo cada segundo
    call GetTickCount
    mov ax, dx
    sub ax, [last_tick]
    cmp ax, 02h          ; Aproximadamente 1 segundo (18.2 ticks por segundo)
    jl skip_move
    call MoveEnemyTowardsPlayer
    call GetTickCount
    mov [last_tick], dx 


skip_move:
    ; Imprimir el enemigo
    mov ah, 02h           ; Posicionar el cursor para el enemigo
    mov bh, 0             ; Página de pantalla
    mov dh, byte ptr [enemigo + 2]   ; Fila del enemigo
    mov dl, byte ptr [enemigo + 1]   ; Columna del enemigo
    int 10h               ; Llama a la interrupción de video BIOS

    ; Imprimir el carácter del enemigo en la posición especificada
    mov ah, 0Ah           ; Función 0Ah de la interrupción 10h: Imprimir carácter en la posición del cursor
    mov al, [charEnemigo] ; Cargar el carácter del enemigo en AL
    mov bh, 0             ; Página de visualización (normalmente es 0)
    mov cx, 1             ; Número de veces que se imprimirá el carácter (en este caso, solo una vez)
    int 10h 

    ; Imprimir el personaje
    mov ah, 02h         ; Posicionar el cursor
    mov bh, 0           ; Página de pantalla
    mov dh, byte ptr [jugador + 1]   ; Fila
    mov dl, byte ptr [jugador]   ; Columna
    int 10h             ; Llama a la interrupción de video BIOS

    ; Imprimir el carácter en la posición especificada
    mov ah, 0Ah      ; Función 0Ah de la interrupción 10h: Imprimir carácter en la posición del cursor
    mov al, char_to_print  ; Cargar el carácter a imprimir en AL
    mov bh, 0        ; Página de visualización (normalmente es 0)
    mov cx, 1        ; Número de veces que se imprimirá el carácter (en este caso, solo una vez)
    int 10h 

    ; Esperar un momento (puedes ajustar esto según sea necesario)
    mov cx, 0FFFFh
wait_loop:
    loop wait_loop

    ret
endp

proc paredes
    xor cx, cx
    cmp ah, '-'
    je choque
    cmp ah, '|'
    je choque
    cmp ah, '+'
    je choque
    jmp fin
    choque:
    mov cx, 1

    fin:
    ret
endp

; Rutina para mover el enemigo hacia el jugador
proc MoveEnemyTowardsPlayer
    ; Calcular la diferencia en X
    push bx
    cmp byte ptr [enemigo + 3], 0
    jne sss
    jmp done
    sss:
    mov al, byte ptr [jugador]
    mov bl, [enemigo + 1]
    sub al, byte ptr [enemigo + 1]
    add al, 1
    jz check_y           ; Si la diferencia en X es 0, chequear Y
    sub al, 2
    js move_left         ; Si la diferencia en X es negativa, moverse a la izquierda
    jmp move_right       ; Si la diferencia en X es positiva, moverse a la derecha

move_right:
    mov bl,byte ptr [enemigo + 1]
    mov al,byte ptr [enemigo + 2]
    call choque_derecha
    mov ah,offset mapa[bx]
    call paredes
    cmp cx ,1
    je done
    inc byte ptr [enemigo + 1]

    jmp done
    
    

move_left:
    mov bl,byte ptr [enemigo + 1]
    mov al,byte ptr [enemigo + 2]
    call choque_izquierda
    mov ah,offset mapa[bx]
    call paredes
    cmp cx ,1
    je done
    dec byte ptr [enemigo + 1]

    jmp done   

check_y:
    ; Calcular la diferencia en Y
    mov al, byte ptr [jugador + 1]
    sub al, byte ptr [enemigo + 2]
    jz done              ; Si la diferencia en Y es 0, no moverse
    js move_up           ; Si la diferencia en Y es negativa, moverse arriba
    jmp move_down        ; Si la diferencia en Y es positiva, moverse abajo

move_up:
    mov al,byte ptr [enemigo + 2]
    call choque_arriba
    mov ah,offset mapa[bx]
    call paredes
    cmp cx ,1
    je done
    dec byte ptr [enemigo + 2]

    jmp done

move_down:
    mov al,byte ptr [enemigo + 2]
    call choque_abajo
    mov ah,offset mapa[bx]
    call paredes
    
    cmp cx ,1
    je done
    inc byte ptr [enemigo + 2]

    jmp done
done:
    call atack
    pop bx
    ret
endp

proc atack
    cmp byte ptr [enemigo + 3], 0
    jna noAtaque
    mov al, byte ptr [jugador + 1]
    sub al, byte ptr [enemigo + 2]
    jz casiAtaque
    jmp noAtaque
casiAtaque:
    mov al, byte ptr [jugador]
    mov bl, [enemigo + 1]
    sub al, byte ptr [enemigo + 1]
    add al, 1
    jz ataque           ; Si la diferencia en X es 0, chequear Y
    sub al, 2
    jz ataque
    jmp noAtaque
ataque:
    mov al, byte ptr [enemigo + 4]   ; Cargar el valor de [enemigo + 4] en el registro AL
    sub byte ptr [jugador + 2], al   ; Restar el valor en AL de [jugador + 2]
noAtaque:    
ret
endp

proc atackPlayer
    ; Código para manejar el ataque del jugador
    mov al, byte ptr [jugador + 1]
    sub al, byte ptr [enemigo + 2]
    jz casiAtaquePlay
    jmp noAtaquePlay
casiAtaquePlay:
    mov al, byte ptr [jugador]
    mov bl, [enemigo + 1]
    sub al, byte ptr [enemigo + 1]
    add al, 1
    jz ataquePlay           ; Si la diferencia en X es 0, chequear Y
    sub al, 2
    jz ataquePlay
    jmp noAtaquePlay
ataquePlay:
    cmp byte ptr [enemigo + 3], 0
    je enemigoYaMuerto
    mov al, byte ptr [jugador + 3]
    sub byte ptr [enemigo + 3], al
    ; Si la vida del enemigo es menor o igual a 0, establecerla a 0
    mov al, byte ptr [enemigo + 3]
    js enemigoYaMuerto
    jmp noAtaquePlay
enemigoYaMuerto:
    mov byte ptr [enemigo + 3], 0
noAtaquePlay:
    ret
endp




end main