;#region Параллелепипед со скругленными краями

;|
  @global
  Функция для создания параллелепипеда со скургленными краями
  @param base_point Отступ от начала координат
  @param body_len Длина параллелепипеда
  @param body_width Ширина параллелепипеда
  @param body_height Высота параллелепипеда
  @param body_rounding Радиус скругления
  @returns Объект параллелепипеда со скругленными краями
|; 
(defun rounded_rect (base_point body_len body_width body_height body_rounding) 
  ; Переношу систему координат
  (command "_ucs" base_point "")
  
  ; Рисую основной параллелепипед
  (command "_box" (list 0 0 0) (list body_len body_width body_height) "")
  (setq mainbox (entlast))
  
  ; Рисую первый вырезаемый бокс
  (draw_box (list 0 0 0) body_rounding body_rounding body_height)
  (command "_subtract" mainbox "" (entlast) "")
  ; Рисую первый добавляемый цилиндр
  (draw_cylinder (list body_rounding body_rounding 0) body_rounding body_height)
  (command "_union" mainbox (entlast) "")
  
  ; Рисую второй вырезаемый бокс
  (draw_box (list (- body_len body_rounding) 0 0) body_rounding body_rounding body_height)
  (command "_subtract" mainbox "" (entlast) "")
  ; Рисую второй добавляемый цилиндр
  (draw_cylinder (list (- body_len body_rounding) body_rounding 0) body_rounding body_height)
  (command "_union" mainbox (entlast) "")
  
  ; Рисую третий вырезаемый бокс
  (draw_box (list 0 (- body_width body_rounding) 0) body_rounding body_rounding body_height)
  (command "_subtract" mainbox "" (entlast) "")
  ; Рисую третий добавляемый цилиндр
  (draw_cylinder (list body_rounding (- body_width body_rounding) 0) body_rounding body_height)
  (command "_union" mainbox (entlast) "")
  
  ; Рисую четвертый вырезаемый бокс
  (draw_box (list (- body_len body_rounding) (- body_width body_rounding) 0) body_rounding body_rounding body_height)
  (command "_subtract" mainbox "" (entlast) "")
  ; Рисую четвертый добавляемый цилиндр
  (draw_cylinder (list (- body_len body_rounding) (- body_width body_rounding) 0) body_rounding body_height)
  (command "_union" mainbox (entlast) "")
  
  ; Переношу систему координат обратно
  (setq neg_base_point (mapcar '- base_point))
  (command "_ucs" neg_base_point "")
)

;|
  @global
  Функция для создания параллелепипеда
  @param base_point Отступ от начала координат
  @param len Длина параллелепипеда
  @param width Ширина параллелепипеда
  @param height Высота параллелепипеда
  @returns Объект параллелепипеда
|; 
(defun draw_box (base_point len width height)
  (command "_ucs" base_point "")
  (command "_box" (list 0 0 0) (list len width height) "")
  (setq neg_base_point (mapcar '- base_point))
  (command "_ucs" neg_base_point "")
)

;|
  @global
  Функция для создания цилиндра
  @param base_point Отступ от начала координат
  @param radius Радиус цилиндра
  @param height Высота цилиндра
  @returns Объект цилиндра
|; 
(defun draw_cylinder (base_point radius height)
  (command "_ucs" base_point "")
  (command "_cylinder" (list 0 0 0) radius (list 0 0 height) "")
  (setq neg_base_point (mapcar '- base_point))
  (command "_ucs" neg_base_point "")
)

;#endregion

;#region Создание кнопки

;|
  @global
  Функция для создания кнопки
  @param base_point Отступ от начала координат
  @param radius Радиус основания кнопки
  @param height Высота кнопки
  @param rounding Радиус скругления кнопки (меньше радиуса)
  @returns Объект кнопки
|; 
(defun draw_button (base_point radius height rounding)
  (command "_ucs" base_point "")
  
  ; Нижний цилиндр
  (command "_circle" (list 0 0 0) radius "")
  (command "_extrude" (entlast) "" (- height rounding) "")
  (setq button_body (entlast))
  
  ; Верхняя крышка  
  (command "_line" (list 0 0 height) (list 0 (- radius rounding) height) "")
  (setq line (entlast))
  (command "_revolve" line "" (list 0 0 0) (list 0 0 1) 360 "")
  (setq upper_surf (entlast))
  (command "_extrude" upper_surf "" (- 0 rounding) "")
  (setq upper_body (entlast))
  
  ; Удаление лишних элементов
  (command "_erase" upper_surf "")
  (command "_erase" line "")
  
  ; Создание четверти круга
  (command "_line" (list 0 0 0) (list 0 rounding 0) "")
  (setq line (entlast))
  (command "_revolve" line "" (list 0 0 0) (list 0 0 1) 360 "")
  (setq full_circle_surf_down (entlast))
  (command "_rectangle" (list rounding rounding 0) (list 0 0 0) "")
  (setq rect_down (entlast))
  (command "_planesurf" "_O" rect_down "")
  (setq rect_surf_down (entlast))
  (command "_intersect" full_circle_surf_down rect_surf_down "")
  (setq quarter_circle (entlast))
  
  ; Удаление лишних элементов
  (command "_erase" line "")
  (command "_erase" rect_down "")
  (command "_erase" full_circle_surf_down "")
  (command "_erase" rect_surf_down "")
  
  ; Поворачиваем поверхность-четверть круга
  (command "_rotate3d" quarter_circle "" (list 0 0 0) (list 0 1 0) 270)
  ; Переносим на нужное место
  (command "_move" quarter_circle "" (list 0 0 0) (list 0 (- radius rounding) (- height rounding)))
  ; Тело округление
  (command "_revolve" quarter_circle "" (list 0 0 0) (list 0 0 1) 360 "")
  (setq rounder_body (entlast))
  
  ; Объединяем все
  (command "_union" button_body upper_body rounder_body "")
    
  (setq neg_base_point (mapcar '- base_point))
  (command "_ucs" neg_base_point "")
)

;#endregion

(defun c:course ()
  ;====================================
  ; Сохранение исходных привязок
  ;====================================
  (setq os (getvar "osmode"))
  (setq ucsfollow (getvar "ucsfollow"))
  (setvar "osmode" 0)
  (setvar "ucsfollow" 0)
  (command "_view" "_save" "myview")
  
  ;====================================
  ; Получение базовой точки
  ;====================================
  (initget 1)
  (setq base_point (getpoint "Укажите базовую точку объекта: "))
  (command "_ucs" base_point "")
  
  ;====================================
  ; Построение главного бокса
  ;====================================
  ; Ввод значений для главного бокса
  (initget (+ 1 2 4))
  (setq body_len (getreal "Укажите длину корпуса: "))
  (initget (+ 1 2 4))
  (setq body_width (getreal "Укажите ширину корпуса: "))
  (initget (+ 1 2 4))
  (setq body_height (getreal "Укажите высоту корпуса: "))
  
  (setq body_rounding nil)
  (while (= body_rounding nil)
    (setq if_checker 1)

    (initget (+ 1 2 4))
    (setq body_rounding (getreal (strcat "Укажите диаметр скругления (x < " (rtos (min body_width body_len))"): ")))
    (if (>= body_rounding body_width)
      (progn
        (setq if_checker 0)
      )
    )
    (if (>= body_rounding body_len)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq body_rounding nil)
    )
  )
  
  ; Построение главного бокса
  (rounded_rect (list 0 0 0) body_len body_width body_height (/ body_rounding 2))
  (setq mainbody_box (entlast))
  
  ;====================================
  ; Построение кнопок
  ;====================================
  ; Ввод значений для массива кнопок
  (initget (+ 1 2 4))
  (setq buttons_count (getreal "Укажите количество кнопок: "))
  
  (setq min_val (/ body_rounding 2))
  (setq max_val (/ body_width 2))
  
  (setq buttons_offset nil)
  (while (= buttons_offset nil)
    (setq if_checker 1)
    (initget (+ 1 2 4))
    (setq buttons_offset (getreal (strcat "Укажите отсутп кнопок от нижней и боковых граней (" (rtos min_val) " < x < " (rtos max_val) "): ")))
    
    (if (< buttons_offset min_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (>= buttons_offset max_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq buttons_offset nil)
    )
  )
  
  (setq min_val (/ (- body_width (* 2 buttons_offset)) (* 2 buttons_count)))
  (setq max_val (- body_len buttons_offset))
  (setq min_val (min min_val max_val))
  
  (setq button_radius nil)
  (while (= button_radius nil)
    (setq if_checker 1)
    (initget (+ 1 2 4))
    (setq button_radius (getreal (strcat "Укажите радиус кнопок (x < " (rtos min_val) "): ")))
    (if (>= button_radius min_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq button_radius nil)
    )
  )
  
  (initget (+ 1 2 4))
  (setq button_height (getreal "Укажите высоту кнопок: "))
  
  (setq max_val button_height)
  (setq max_val (min max_val button_radius))
  
  (setq button_rounding_diameter nil)
  (while (= button_rounding_diameter nil)
    (setq if_checker 1)
    (initget (+ 1 2 4))
    (setq button_rounding_diameter (getreal (strcat "Укажите диаметр закруглений краёв кнопок (x <= " (rtos max_val) "): ")))
    (if (> button_rounding_diameter max_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq button_rounding_diameter nil)
    )
  )
  (setq button_rounding_radius (/ button_rounding_diameter 2))
  
  ; Строим массив
  (setq pt1 (list (- body_len buttons_offset) buttons_offset body_height))
  (setq pt2 (list (- body_len buttons_offset) (- body_width buttons_offset) body_height))
  
  (setq dist (distance pt1 pt2))
  (setq dist (- dist (* 2 button_radius)))
  (setq dist (/ dist (1- buttons_count)))
  (setq new_obj_count buttons_count)
   
  ; Каждый раз надо рисовать новую кнопку
  (setq lastobj button_body)
  (setq offset 0)
  (while (> new_obj_count 0)
    (setq new_obj_count (- new_obj_count 1))
    (draw_button (list (- (- body_len button_radius) buttons_offset) (+ buttons_offset (+ button_radius offset)) body_height) button_radius button_height button_rounding_radius)
    (setq offset (+ offset dist))
    (command "_union" mainbody_box (entlast) "")
    (setq lastobj lastobj2)
  )
  
  ;====================================
  ; Построение экрана
  ;====================================
  ; Ввод значений
  (setq max_val (/ body_width 2))
  (setq max_val (min max_val (/ (- body_len (+ buttons_offset (* 2 button_radius))) 2)))
  (setq min_val (/ body_rounding 2))
  (setq screen_offset nil)
  (while (= screen_offset nil)
    (setq if_checker 1)
    
    (initget (+ 1 2 4))
    (setq screen_offset (getreal (strcat "Укажите отступ экрана от кнопок, боковых и верхней граней (" (rtos min_val) " <= x < " (rtos max_val) "): ")))
    (if (>= screen_offset max_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (< screen_offset min_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq screen_offset nil)
    )
  )
  
  (setq screen_len (- body_len (+ (* 2 screen_offset) (+ buttons_offset (* 2 button_radius)))))
  (setq screen_width (- body_width (* 2 screen_offset)))
  
  (initget (+ 1 2 4))
  (setq screen_height (getreal "Укажите высоту экрана: "))
  
  (setq max_val (min screen_width screen_len))
  
  (setq screen_rounding nil)
  (while (= screen_rounding nil)
    (setq if_checker 1)
    (initget (+ 1 2 4))
    (setq screen_rounding (getreal (strcat "Укажите диаметр округления экрана (x <= " (rtos max_val) ") : " )))
    (if (>= screen_rounding max_val)
      (progn
        (setq if_checker 0)
      )
    )
    (if (= if_checker 0)
      (setq screen_rounding nil)
    )
  )
  ; Построение экрана
  (rounded_rect (list screen_offset screen_offset body_height) screen_len screen_width screen_height (/ screen_rounding 2))
  (command "_union" mainbody_box (entlast) "")
  
  
  ;======================================
  ; Возвращение исходных привязок
  ;======================================
  (setvar "osmode" os)
  (setvar "ucsfollow" ucsfollow)
  (command "_view" "_restore" "myview")
  (command "_view" "_delete" "myview")
)