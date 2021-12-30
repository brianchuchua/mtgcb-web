import { useRouter } from 'next/router';
import { useEffect } from 'react';
import { useAuthentication } from '../auth/AuthenticationProvider';
import { logOut } from '../network/features/login';

const logOutOfMtgCb = async () => {
  const response = {
    data: null,
    error: null,
  };

  const result = await logOut();

  const logOutWasSuccessful = result?.data?.data?.unauthenticate?.success;
  if (logOutWasSuccessful) {
    response.data = {
      success: true,
    };
  } else {
    response.data = {
      success: false,
    };
    response.error = 'There was a problem logging out.';
  }

  return response;
};

const LogoutPage: React.FC = () => {
  const router = useRouter();
  const { isAuthenticated, isCheckingAuth, setUser } = useAuthentication();

  useEffect(() => {
    async function triggerLogOut() {
      if (!isAuthenticated && !isCheckingAuth) {
        router.push('/');
      } else {
        await logOutOfMtgCb();
        setUser(null);
        router.push('/');
      }
    }
    triggerLogOut();
  }, [isAuthenticated, isCheckingAuth, router, setUser]);

  return <></>;
};

export default LogoutPage;
