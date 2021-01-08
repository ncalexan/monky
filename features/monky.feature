Feature: Do Some things
  In order to do something
  As a user
  I want to do something

  # (Before
  #   (let ((default-directory (f-join (temporary-file-directory) (format "monky-%s" (* 0 (time-convert nil 'integer))))))
  #   (shell-command "hg init")
  #   (monky-status)))

  Scenario: Do Something
    # (Given "^I start with a fresh repository$"
    #    (lambda ()
    #      (let ((default-directory (f-join (temporary-file-directory) (format "monky-%s" (* 0 (time-convert nil 'integer))))))
    #        (shell-command "hg init")
    #        (monky-status))))
    When I start with a fresh repository
    (cl-assert (< 2 1))
    Then I should be in buffer "*monky: new*"
    Then I should see:
    """
    Changes:
    """

    # Then I should see command output:

    # (Then "^I should see command output:$"
    #   (lambda (expected)
    #   (ecukes-should-match expected ecukes-stdout)))

    # Given I am in buffer "*monky: monky-0*"
    # Then I should see "test2"
    # When I have "something"
    # Then I should have "something"
    # And I should have "something"
    # But I should not have "something"
