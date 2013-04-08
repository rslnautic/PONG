;;; Copyright (c) 2012 by Ramón Serrano López
;;; Test for Cairo with OpenGL
(define game-state 'init)
(define move-player1 'non)
(define move-player2 'non)
(define space-pressed? #f)
(define r-pressed? #f)
(define goal? #f)
(define player1x 0.0)
(define player1y 320.0)
(define player2x 1240.0)
(define player2y 320.0)
(define ballx 50.0)
(define bally 370.0)
(define score1 0)
(define score2 0)
(define speed-player 0.5)
(define speed-ball-x 5)
(define speed-ball-y 5)
(define time-before 0)
(define y-final 0)


(define (main)
  ((fusion:create-simple-gl-cairo '(width: 1280 height: 752))
   (lambda (event world)
     ;(println (string-append "event: " (object->string event) " ; world: " (object->string world)))
     (let ((type (SDL_Event-type event)))
       (cond
        ((= type SDL_QUIT)
         'exit)
        ((= type SDL_MOUSEBUTTONDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Button down")
         'new-world-modified-by-mouse-button-event)
        ((= type SDL_KEYDOWN)
         (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Key down")
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym
                      (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_ESCAPE)
                  'exit)
                 ((= key SDLK_SPACE)
                  (set! space-pressed? #t))
                 ((= key SDLK_r)
                  (set! r-pressed? #t))
                 ((= key SDLK_UP)
                  (set! move-player2 'up)
                  ;(set! player2y (- player2y 20))
                  )
                 ((= key SDLK_DOWN)
                  (set! move-player2 'down)
                  ;(set! player2y (+ player2y 20))
                  )
                 ((= key SDLK_w)
                  (set! move-player1 'up)
                  ;(set! player1y (- player1y 20))
                  )
                 ((= key SDLK_s)
                  (set! move-player1 'down)
                  ;(set! player1y (+ player1y 20))
                  )
                 ((= key SDLK_1)
                  (set! score1 (+ score1 1)))
                 ((= key SDLK_2)
                  (set! score2 (+ score2 1)))
                 (else
                  (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))
                  'new-world-modified-by-key-event))))

        ((= type SDL_KEYUP)
         (let* ((kevt (SDL_Event-key event))
                (key (SDL_Keysym-sym (SDL_KeyboardEvent-keysym kevt))))
           (cond ((= key SDLK_ESCAPE)
                  'exit)
                 ((= key SDLK_UP)
                  (set! move-player2 'none))
                 ((= key SDLK_DOWN)
                  (set! move-player2 'none))
                 ((= key SDLK_w)
                  (set! move-player1 'none))
                 ((= key SDLK_s)
                  (set! move-player1 'none))
                 (else
                  ;(add-key-element key)
                  (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (string-append "Key: " (number->string key)))))))

        ((= type SDL_WINDOWEVENT)
         (let* ((wevt (SDL_Event-window event))
                (event (SDL_WindowEvent-event wevt)))
           (cond
            ((= event SDL_WINDOWEVENT_SIZE_CHANGED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Size Changed"))
            ((= event SDL_WINDOWEVENT_RESIZED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Resized"))
            ((= event SDL_WINDOWEVENT_MINIMIZED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Minimized"))
            ((= event SDL_WINDOWEVENT_RESTORED)
             (SDL_LogVerbose SDL_LOG_CATEGORY_APPLICATION "Window Restored"))))
         'new-world-modified-by-window-event)
        ((= type SDL_FINGERDOWN)
         (SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION "FINGER DOWN!")
         'new-world-modified-by-finger-event)
        (else
         'new-world-modified-by-event))))
   (let ((posx 80.0))
     (lambda (cr time world)
       
       (pp (- time time-before))
       
       ;(println (string-append "time: " (object->string time) " ; world: " (object->string world)))
       ;;(SDL_LogInfo SDL_LOG_CATEGORY_APPLICATION (object->string (SDL_GL_Extension_Supported "GL_EXT_texture_format_BGRA8888")))
       (cairo_set_source_rgba cr 0.0 0.0 0.0 1.0)
       (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
       (cairo_fill cr)
       ;;(cairo_arc cr posx 80.0 150.0 0.0 6.28)
       ;;(cairo_set_source_rgba cr 0.0 0.0 1.0 1.0)
       ;;(cairo_fill cr)
       ;;(cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
       ;(cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
       ;(cairo_set_font_size cr 200.0)
       ;(cairo_move_to cr 300.0 400.0)
       ;(cairo_show_text cr "PONG")
       ;(cairo_fill cr)
       ;(cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
       ;(cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
       ;(cairo_set_font_size cr 50.0)
       ;(cairo_move_to cr 80.0 600.0)
       ;(cairo_show_text cr "¡PRESS SPACE BAR TO START THE GAME!")
       ;(cairo_fill cr)
       ;(set! posx (+ 1.0 posx))

       ;;STATES MOVE-PLAYER1
       
   
       (case move-player1
         ((up)
          (when (> player1y 0)
                (set! player1y (- player1y (* speed-player (- time time-before))))))
         ((down)
          (when (< player1y 652)
                (set! player1y (+ player1y (* speed-player (- time time-before)))))))
       

       ;;STATES MOVE-PLAYER2

       (case move-player2
         ((up)
          (when (> player2y 0)
                (set! player2y (- player2y (* speed-player (- time time-before))))))
         ((down)
          (when (< player2y 652)
                (set! player2y (+ player2y (* speed-player (- time time-before)))))))


       ;;STATES-GAME
       
       (case game-state
         ;; State: init
         ((init)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 400.0)
          (cairo_move_to cr 0.0 400.0)
          (cairo_show_text cr "PONG")
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 55.0)
          (cairo_move_to cr 00.0 600.0)
          (cairo_show_text cr " PRESS SPACE BAR TO START THE GAME!")
          (cairo_fill cr)
          (when space-pressed?
                (set! r-pressed? #f)
                (set! game-state 'start-screen)))


         ;; State: start-screen
         ((start-screen)
          
          (set! space-pressed? #f)            
          ;; FONDO NEGRO
          (cairo_set_source_rgba cr 0.0  0.0 0.0 1.0)
          (cairo_rectangle cr 0.0 0.0 1280.0 752.0)
          (cairo_fill cr)

          ;;TIEMPO
          ;;(cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          ;;(cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          ;;(cairo_set_font_size cr 50.0)
          ;;(cairo_move_to cr 50.0 50.0)
          ;;(cairo_show_text cr (object->string  time))
          ;;(cairo_fill cr)

          ;;BARRA CENTRAL
          (cairo_set_source_rgba cr 1.0  1.0 1.0 1.0)
          (cairo_rectangle cr 620.0 0.0 20.0 752.0)
          (cairo_fill cr)

          ;;--------------PLAYER 1-------------------------------------------
          (cairo_set_source_rgba cr 1.0  1.0 1.0 1.0)
          (cairo_rectangle cr player1x player1y 40.0 100.0)
          (cairo_fill cr) 
          
          ;;PLAYER 1 --> SCORE
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 100.0)
          (cairo_move_to cr 550.0 100.0)
          (cairo_show_text cr (object->string  score1))
          (cairo_fill cr)
          
          ;;------------PLAYER 2-------------------------------------------
          (cairo_set_source_rgba cr 1.0  1.0 1.0 1.0)
          (cairo_rectangle cr player2x player2y 40.0 100.0)
          (cairo_fill cr)

          ;;PLAYER 2 --> SCORE
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 100.0)
          (cairo_move_to cr 640.0 100.0)
          (cairo_show_text cr (object->string  score2))
          (cairo_fill cr)

          ;;-----------------BALL-------------------------------------------

          ;;REBOTES

          (set! ballx (+ ballx speed-ball-x))
          (set! bally (- bally speed-ball-y))
          (when (< bally 10)
                (set! speed-ball-y (- speed-ball-y)))
          (when (> bally 742)
                (set! speed-ball-y (- speed-ball-y)))
          
          #;
          (when (> ballx player2x)
                (set! speedx (- speedx)))
          #;
          (when (< ballx (+ player1x 50))
                (set! speedx (- speedx)))
          
          (if (and (> ballx player2x) (> bally player2y) (< bally (+ player2y 100)))
              (set! speed-ball-x (- speed-ball-x)))

          (if (and (< ballx (+ player1x 50)) (> bally player1y) (< bally (+ player1y 100)))
              (set! speed-ball-x (- speed-ball-x)))

          ;;PUNTUACION
          (when (< ballx 0)
                (set! score2 (+ score2 1))
                (set! ballx 50.0)
                (set! bally 370.0)
                (set! speed-ball-x 5)
                (set! speed-ball-y 5)
                (set! player1x 0.0)
                (set! player1y 320.0)
                (set! player2x 1240.0)
                (set! player2y 320.0))

          (when (> ballx 1280)
                (set! score1 (+ score1 1))
                (set! ballx 50.0)
                (set! bally 370.0)
                (set! speed-ball-x 5)
                (set! speed-ball-y 5)
                (set! player1x 0.0)
                (set! player1y 320.0)
                (set! player2x 1240.0)
                (set! player2y 320.0))



          ;;PINTAR FORMA CUADRADA
          ;;(cairo_set_source_rgba cr 0.0  1.0 1.0 1.0)
          ;;(cairo_rectangle cr ballx bally 30.0 30.0)
          ;;(cairo_fill cr)
          
          ;;PINTAR FORMA REDONDA
          (cairo_arc cr ballx bally 10.0 0.0 10.0)
          (cairo_set_source_rgba cr 0.0 1.0 1.0 1.0)
          (cairo_fill cr)
          
          
          ;;-----------CAMBIO DE ESTADO GANADOR-----------------------------
          (when (= score1 7)
                (set! game-state 'player1win)
                (set! score1 (- score1 score1))
                (set! score2 (- score2 score2)))

          (when (= score2 7)
                (set! game-state 'player2win)
                (set! score1 (- score1 score1))
                (set! score2 (- score2 score2))))
         
         ((player1win)
          
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 200.0)
          (cairo_move_to cr 300.0 400.0)
          (cairo_show_text cr "WINS")
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 50.0)
          (cairo_move_to cr 0.0 500.0)
          (cairo_show_text cr "PLAYER1 PRESS KEY R TO REBOOT THE GAME")
          (cairo_fill cr)
          (when r-pressed?
                (set! game-state 'init)
                (set! ballx 50.0)
                (set! bally 370.0)))
         
         ((player2win)
          
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 200.0)
          (cairo_move_to cr 300.0 400.0)
          (cairo_show_text cr "WINS")
          (cairo_fill cr)
          (cairo_select_font_face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
          (cairo_set_source_rgba cr 1.0 1.0 1.0 1.0)
          (cairo_set_font_size cr 50.0)
          (cairo_move_to cr 0.0 500.0)
          (cairo_show_text cr "PLAYER2 PRESS KEY R TO REBOOT THE GAME")
          (cairo_fill cr)
          (when r-pressed?
                (set! game-state 'init)
                (set! ballx 50.0)
                (set! bally 370.0))))
       
       (set! time-before time)
       'new-world-modified-by-draw))
   'default-world))

