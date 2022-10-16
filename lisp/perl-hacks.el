(require 'format-all)

(define-format-all-formatter perltidy
  (:executable "perltidy")
  (:install "cpan install Perl::Tidy")
  (:languages "Perl")
  (:features)
  (:format (format-all--buffer-easy
            executable
            (let ((perltidyrc (format-all--locate-file ".perltidyrc")))
              (when perltidyrc (concat "--profile=" perltidyrc))))))

(add-hook 'perl-mode-hook 'format-all-mode)
