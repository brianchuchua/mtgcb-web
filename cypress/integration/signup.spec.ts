import { v4 as uuidv4 } from 'uuid';

describe('The Sign Up Page', () => {
  it('successfully loads if unauthenticated', () => {
    cy.visit('/signup');
  });

  it('has a log in link', () => {
    cy.visit('/signup');

    cy.get('#signup-form').contains('Already have an account? Log In');
  });

  it('allows user to sign up', () => {
    cy.fixture('accounts/newUser').then((newUser) => {
      const { username, email, password } = newUser;

      const randomUsername = `${username}-${uuidv4()}`;
      cy.visit('/signup');

      cy.get('input[name=username]').type(randomUsername);
      cy.get('input[name=email]').type(email);
      cy.get('input[name=password]').type(password);
      cy.get('input[name=passwordConfirmation]').type(password);

      cy.get('button[type=submit]').click();

      cy.get('#account-menu').contains('Log Out');
    });
  });

  it('redirects to home if authenticated', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.login(username, password);

      cy.visit('/signup');

      cy.url().should('not.include', '/signup');
      cy.get('#account-menu').contains('Log Out');
    });
  });

  it('shows errors if fields are missing', () => {
    cy.visit('/signup');

    cy.get('button[type=submit]').click();

    cy.url().should('include', '/signup');
    cy.get('#signup-form').contains('Username is required');
    cy.get('#signup-form').contains('Email is required');
    cy.get('#signup-form').contains('Password is required');
    cy.get('#signup-form').contains('Password confirmation is required');
  });

  it('shows an error if email is invalid', () => {
    cy.fixture('accounts/newUser').then((newUser) => {
      const { username, password } = newUser;
      const invalidEmail = 'NotAValidEmailAddress';

      cy.visit('/signup');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=email]').type(invalidEmail);
      cy.get('input[name=password]').type(password);
      cy.get('input[name=passwordConfirmation]').type(password);

      cy.get('button[type=submit]').click();

      cy.url().should('include', '/signup');
      cy.get('#signup-form').contains('Valid email is required');
    });
  });

  it('shows an error if password is too short', () => {
    cy.fixture('accounts/newUser').then((newUser) => {
      const { username, email } = newUser;
      const passwordThatIsTooShort = 'abc';

      cy.visit('/signup');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=email]').type(email);
      cy.get('input[name=password]').type(passwordThatIsTooShort);
      cy.get('input[name=passwordConfirmation]').type(passwordThatIsTooShort);

      cy.get('button[type=submit]').click();

      cy.url().should('include', '/signup');
      cy.get('#signup-form').contains('Password must be at least eight characters long');
    });
  });

  it('shows an error if password does not match confirmation', () => {
    cy.fixture('accounts/newUser').then((newUser) => {
      const { username, email } = newUser;
      const password = 'password';
      const mismatchedPassword = 'passwordd';

      cy.visit('/signup');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=email]').type(email);
      cy.get('input[name=password]').type(password);
      cy.get('input[name=passwordConfirmation]').type(mismatchedPassword);

      cy.get('button[type=submit]').click();

      cy.url().should('include', '/signup');
      cy.get('#signup-form').contains('Passwords must match');
    });
  });

  it('shows an error if the username is already in use', () => {
    cy.fixture('accounts/normalUser').then((existingUser) => {
      const { username, password } = existingUser;
      const email = 'user@example.com';

      cy.visit('/signup');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=email]').type(email);
      cy.get('input[name=password]').type(password);
      cy.get('input[name=passwordConfirmation]').type(password);

      cy.get('button[type=submit]').click();

      cy.url().should('include', '/signup');
      cy.get('#signup-form').contains('That username is already in use. Please choose another.');
    });
  });
});
