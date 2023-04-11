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
  @param rounding Радиус скругления кнопки
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
(defun c:draw_button_test ()
  (draw_button (list 0 0 0) 10 15 7)
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
  (setq body_width (getreal "Укажите ширину корпуса: "))
  (setq body_height (getreal "Укажите высоту корпуса: "))
  (setq body_rounding (getreal "Укажите радиус скругления: "))
  ; Построение главного бокса
  (rounded_rect (list 0 0 0) body_len body_width body_height body_rounding)
  (setq mainbox (entlast))
  
  ;====================================
  ; Построение главного бокса
  ;====================================
  ; Ввод значений для массива кнопок
  (initget (+ 1 2 4))
  (setq buttons_count (getreal "Укажите количество кнопок: "))
  ; Надо сделать проверку, чтобы отступ от нижней был не оч большой, а от боковых - не пересекался
  (setq buttons_offset (getreal "Укажите отсутп кнопок от нижней и боковых граней: "))
  ; Зная предыдущие данные, нужно посчитать, сколько кнопок максимум, чтобы хотябы был зазор
  (setq button_radius (getreal "Укажите радиус кнопок: "))
  (setq button_height (getreal "Укажите высоту кнопок: "))
  ; Нужно будет делить на 2, тк в методе радиус
  (setq button_rounding_diameter (getreal "Укажите диаметр закруглений краёв кнопок: "))
    
  
    
  ;======================================
  ; Возвращение исходных привязок
  ;======================================
  (setvar "osmode" os)
  (setvar "ucsfollow" ucsfollow)
  (command "_view" "_restore" "myview")
  (command "_view" "_delete" "myview")
  (command "zoom" "e")
)