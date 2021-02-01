const signUp = `mutation signUp($username: String!, $email: String!, $password: String!, $passwordConfirmation: String!) {
  signUp(username: $username, email: $email, password: $password, passwordConfirmation: $passwordConfirmation) {
    item {
      id
      username
    }
  }
}
`;

export default signUp;
