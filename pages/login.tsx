import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';
import { Login } from '../features/login/Login';

const LoginPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xs">
    <Login />
  </ResponsiveContainer>
);

export default LoginPage;
