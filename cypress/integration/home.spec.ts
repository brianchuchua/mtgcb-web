describe('The Home Page', () => {
  it('successfully loads', () => {
    cy.visit('/');
  });

  it('has a link to the login page if unauthenticated', () => {
    cy.visit('/');

    cy.get('#account-menu').contains('Log In');
  });

  it('has a link to logout if authenticated', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.login(username, password);

      cy.visit('/');

      cy.get('#account-menu').contains('Log Out');
    });
  });
});
