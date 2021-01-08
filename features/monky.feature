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
    Then I should be in buffer "*monky: new*"
    Then I should see:
    """
    Changes:
    """
    When I run shell "touch x && hg add x"
    When I run shell "echo 0 >> x && hg commit -m '0'"
    When I run shell "echo 1 >> x && hg commit -m '1'"
    When I run shell "echo 2 >> x && hg commit -m '2'"
    When I run shell "hg up 0"
    When I run shell "echo 3 >> x && hg commit -m '3'"
    When I run shell "echo 4 >> x && hg commit -m '4'"
    When I run shell "hg log -G"
    Then I should see command output:
    """
    @  changeset:   4:d9fa7e37fffa
    |  tag:         tip
    |  user:        test
    |  date:        Thu Jan 01 00:00:00 1970 +0000
    |  summary:     4
    |
    o  changeset:   3:25ed43dbf5ce
    |  parent:      0:fc1a7f20d88e
    |  user:        test
    |  date:        Thu Jan 01 00:00:00 1970 +0000
    |  summary:     3
    |
    | o  changeset:   2:7b52c2647b25
    | |  user:        test
    | |  date:        Thu Jan 01 00:00:00 1970 +0000
    | |  summary:     2
    | |
    | o  changeset:   1:24a608e4ce05
    |/   user:        test
    |    date:        Thu Jan 01 00:00:00 1970 +0000
    |    summary:     1
    |
    o  changeset:   0:fc1a7f20d88e
       user:        test
       date:        Thu Jan 01 00:00:00 1970 +0000
       summary:     0
    """
    When I run shell "hg rebase -s 1 -d 4"
    When I run shell "hg status"
    Then I should see command output:
    """
    0
    3
    4
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
