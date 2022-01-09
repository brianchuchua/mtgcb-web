import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';
import { SignUp } from '../features/signup/SignUp';

const SignUpPage: React.FC = () => (
  <ResponsiveContainer maxWidth="xs">
    <SignUp />
  </ResponsiveContainer>
);

export default SignUpPage;
