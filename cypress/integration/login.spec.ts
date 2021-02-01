describe('The Login Page', () => {
  it('successfully loads if unauthenticated', () => {
    cy.visit('/login');
  });

  it('has a forgot password link', () => {
    cy.visit('/login');

    cy.get('#login-form').contains('Forgot password?');
  });

  it('has a sign up link', () => {
    cy.visit('/login');

    cy.get('#login-form').contains("Don't have an account? Sign Up");
  });

  it('allows user to login', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.visit('/login');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=password]').type(password);
      cy.get('button[type=submit]').click();

      cy.get('#account-menu').contains('Log Out');
    });
  });

  it('redirects to home if authenticated', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.login(username, password);

      cy.visit('/login');

      cy.url().should('not.include', '/login');
      cy.get('#account-menu').contains('Log Out');
    });
  });

  it('shows errors if fields are missing', () => {
    cy.visit('/login');

    cy.get('button[type=submit]').click();

    cy.url().should('include', '/login');
    cy.get('#login-form').contains('Username is required');
    cy.get('#login-form').contains('Password is required');
  });

  it('shows a generic error if username is incorrect', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { password } = user;
      const incorrectUsername = 'Fblthp';

      cy.visit('/login');

      cy.get('input[name=username]').type(incorrectUsername);
      cy.get('input[name=password]').type(password);
      cy.get('button[type=submit]').click();

      cy.url().should('include', '/login');
      cy.get('#login-form').contains('Invalid username or password');
    });
  });

  it('shows a generic error if password is incorrect', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username } = user;
      const incorrectPassword = 'functionally unique secret lair cards are a good idea';

      cy.visit('/login');

      cy.get('input[name=username]').type(username);
      cy.get('input[name=password]').type(incorrectPassword);
      cy.get('button[type=submit]').click();

      cy.url().should('include', '/login');
      cy.get('#login-form').contains('Invalid username or password');
    });
  });
});
