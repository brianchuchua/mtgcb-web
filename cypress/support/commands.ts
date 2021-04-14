import { authenticateUserWithPassword } from '../../network/mutations';

Cypress.Commands.add('login', (username, password) => {
  cy.request({
    method: 'POST',
    url: Cypress.env('NEXT_PUBLIC_CYPRESS_MTGCB_API_URL'),
    body: {
      query: authenticateUserWithPassword,
      variables: {
        username,
        password,
      },
    },
  });
});
