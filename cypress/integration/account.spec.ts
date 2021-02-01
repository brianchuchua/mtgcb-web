describe('The Account Page', () => {
  it('redirects to home if unauthenticated', () => {
    cy.visit('/account');

    cy.url().should('not.include', '/account');
    cy.get('#account-menu').contains('Log In');
  });

  it('starts in the general tab', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.login(username, password);
      cy.visit('/account');

      cy.get('#tab-0').contains('General');
      cy.get('#tab-0').should('have.attr', 'aria-selected', 'true');
    });
  });

  describe('The General Tab', () => {
    it("shows the user's current username and email", () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password, email } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=username]').should('have.value', username);
        cy.get('input[name=email]').should('have.value', email);
      });
    });

    it('disables the save button if there are no changes', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password, email } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('button[type=submit]').should('be.disabled');

        cy.get('input[name=username]').type('2');
        cy.get('input[name=email]').type('.br');
        cy.get('input[name=username]').clear().type(username);
        cy.get('input[name=email]').clear().type(email);

        cy.get('button[type=submit]').should('be.disabled');
      });
    });

    it('enables the save button if there are changes', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=username]').type(' 2');
        cy.get('input[name=email]').type('.br');

        cy.get('button[type=submit]').should('not.be.disabled');
      });
    });

    it('enables the user to update their username', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password, email } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=username]').type(' 3');
        cy.get('button[type=submit]').click();

        cy.get('#client-snackbar').contains('Your profile has been updated');
        cy.reload();
        cy.get('input[name=username]').should('have.value', `${username} 3`);
        cy.get('input[name=email]').should('have.value', email);

        cy.get('input[name=username]').clear().type(username);
        cy.get('button[type=submit]').click();
        cy.get('#client-snackbar').contains('Your profile has been updated');
        cy.reload();
        cy.get('input[name=username]').should('have.value', username);
        cy.get('input[name=email]').should('have.value', email);
      });
    });

    it('shows an error if username already exists', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.fixture('accounts/normalUser2').then((existingUser) => {
          const { username: existingUsername } = existingUser;

          cy.get('input[name=username]').clear().type(existingUsername);
          cy.get('button[type=submit]').click();

          cy.get('#update-profile-form').contains('That username is already in use. Please choose another.');
        });
      });
    });

    it('enables the user to update their email', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password, email } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=email]').type('.br');
        cy.get('button[type=submit]').click();

        cy.get('#client-snackbar').contains('Your profile has been updated');
        cy.reload();
        cy.get('input[name=username]').should('have.value', username);
        cy.get('input[name=email]').should('have.value', `${email}.br`);

        cy.get('input[name=email]').clear().type(email);
        cy.get('button[type=submit]').click();

        cy.get('#client-snackbar').contains('Your profile has been updated');
        cy.reload();
        cy.get('input[name=username]').should('have.value', username);
        cy.get('input[name=email]').should('have.value', email);
      });
    });

    it('shows errors if fields are missing', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=username]').clear().click();
        cy.get('input[name=email]').clear().click();
        cy.get('input[name=username]').click();

        cy.get('#update-profile-form').contains('Username is required');
        cy.get('#update-profile-form').contains('Email is required');
      });
    });

    it('shows an error if email is invalid', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;
        const invalidEmail = 'NotAValidEmailAddress';

        cy.login(username, password);
        cy.visit('/account');

        cy.get('input[name=email]').clear().type(invalidEmail);
        cy.get('input[name=username]').click();

        cy.get('#update-profile-form').contains('Valid email is required');
      });
    });
  });

  describe('The Password Tab', () => {
    it('navigates to the password tab', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');

        cy.get('#tab-1').contains('Password');
        cy.get('#tab-1').click();
        cy.get('#tab-1').should('have.attr', 'aria-selected', 'true');
      });
    });

    it('shows the two blank reset password fields', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');
        cy.get('#tab-1').click();

        cy.get('input[name=password]').should('have.value', '');
        cy.get('input[name=passwordConfirmation]').should('have.value', '');
      });
    });

    it('disables the save button if there are no changes', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');
        cy.get('#tab-1').click();

        cy.get('button[type=submit]').should('be.disabled');

        cy.get('input[name=password]').type('jacethepasswordsculptor');
        cy.get('input[name=passwordConfirmation]').type('jacethepasswordsculptor');
        cy.get('input[name=password]').clear();
        cy.get('input[name=passwordConfirmation]').clear();

        cy.get('button[type=submit]').should('be.disabled');
      });
    });

    it('enables the save button if there are changes', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');
        cy.get('#tab-1').click();

        cy.get('input[name=password]').type('jacethepasswordsculptor');
        cy.get('input[name=passwordConfirmation]').type('jacethepasswordsculptor');

        cy.get('button[type=submit]').should('not.be.disabled');
      });
    });

    it('shows an error if password is too short', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');
        cy.get('#tab-1').click();

        cy.get('input[name=password]').type('pass');
        cy.get('input[name=passwordConfirmation]').type('pass');
        cy.get('input[name=password]').click();

        cy.get('button[type=submit]').should('be.disabled');
        cy.get('#update-password-form').contains('Password must be at least eight characters long');
        cy.get('#update-password-form').contains('Password confirmation must be at least eight characters long');
      });
    });

    it('shows an error if password does not match confirmation', () => {
      cy.fixture('accounts/normalUser').then((user) => {
        const { username, password } = user;

        cy.login(username, password);
        cy.visit('/account');
        cy.get('#tab-1').click();

        cy.get('input[name=password]').type('jacethepasswordsculptor');
        cy.get('input[name=passwordConfirmation]').type('chandrathetypoqueen');
        cy.get('input[name=password]').click();

        cy.get('button[type=submit]').should('be.disabled');
        cy.get('#update-password-form').contains('Passwords must match');
      });
    });
  });
});
