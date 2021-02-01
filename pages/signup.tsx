import Container from '@material-ui/core/Container';
import { SignUp } from '../features/signup/SignUp';

const SignUpPage: React.FC = () => (
  <Container component="main" maxWidth="xs">
    <SignUp />
  </Container>
);

export default SignUpPage;
