describe('The Logout Route', () => {
  it('redirects to home if unauthenticated', () => {
    cy.visit('/logout');

    cy.url().should('not.include', '/logout');
    cy.get('#account-menu').contains('Log In');
  });

  it('logs the user out', () => {
    cy.fixture('accounts/normalUser').then((user) => {
      const { username, password } = user;

      cy.login(username, password);
      cy.visit('/logout');

      cy.url().should('not.include', '/logout');
      cy.get('#account-menu').contains('Log In');
    });
  });
});
