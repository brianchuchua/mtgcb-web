import Container from '@material-ui/core/Container';
import { Login } from '../features/login/Login';

const LoginPage: React.FC = () => (
  <Container component="main" maxWidth="xs">
    <Login />
  </Container>
);

export default LoginPage;
