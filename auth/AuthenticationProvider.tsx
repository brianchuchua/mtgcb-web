import { useRouter } from 'next/router';
import React, { createContext, SetStateAction, useContext, useEffect, useState } from 'react';
import { getUser } from '../network/features/account';

export interface User {
  id?: string;
  username?: string;
  email?: string;
}

export interface Authentication {
  isAuthenticated: boolean;
  user: User;
  setUser: React.Dispatch<SetStateAction<User>>;
  isCheckingAuth: boolean;
}

const AuthenticationContext = createContext<Partial<Authentication>>({});

export const AuthenticationProvider: React.FC = ({ children }) => {
  const [user, setUser] = useState(null);
  const [isCheckingAuth, setIsCheckingAuth] = useState(true);

  useEffect(() => {
    async function checkIfUserIsAuthenticated() {
      const result = await getUser();
      if (result?.data?.data?.authenticatedUser) {
        setUser(result?.data?.data?.authenticatedUser);
      }
      setIsCheckingAuth(false);
    }
    checkIfUserIsAuthenticated();
  }, []);

  return (
    <AuthenticationContext.Provider value={{ isAuthenticated: !!user, user, setUser, isCheckingAuth }}>
      {children}
    </AuthenticationContext.Provider>
  );
};

export const useAuthentication = (): Partial<Authentication> => {
  const context = useContext(AuthenticationContext);
  return context;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const privateRoute: any = (Component) => () => {
  const { isAuthenticated, isCheckingAuth } = useAuthentication();

  const router = useRouter();

  useEffect(() => {
    if (!isCheckingAuth && !isAuthenticated) {
      const destination = router.asPath;
      router.push(`/login?destination=${destination}`);
    }
  }, [isCheckingAuth, isAuthenticated, router]);

  return <Component />;
};
