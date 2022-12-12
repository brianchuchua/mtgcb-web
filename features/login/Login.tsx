import Avatar from '@material-ui/core/Avatar';
import Box from '@material-ui/core/Box';
import FormHelperText from '@material-ui/core/FormHelperText';
import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import Typography from '@material-ui/core/Typography';
import LockOutlinedIcon from '@material-ui/icons/LockOutlined';
import { Field, Form, Formik } from 'formik';
import { useRouter } from 'next/router';
import styled from 'styled-components';
import * as Yup from 'yup';
import { useAuthentication } from '../../auth/AuthenticationProvider';
import Button from '../../components/Button';
import Link from '../../components/Link';
import { getUser } from '../../network/features/account';
import { logIn, migrateLegacyAccount } from '../../network/features/login';

const LoginWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(8),
  display: 'flex',
  flexDirection: 'column',
  alignItems: 'center',
}));

const LoginIcon = styled(Avatar)(({ theme }) => ({
  margin: theme.spacing(1),
  backgroundColor: theme.palette.primary.main,
}));

const LoginForm = styled(Form)(({ theme }) => ({
  width: '100%',
  marginTop: theme.spacing(1),
}));

const SubmitButtonWrapper = styled.div(({ theme }) => ({
  margin: theme.spacing(3, 0, 2),
}));

const LoginSchema = Yup.object().shape({
  username: Yup.string().required('Username is required'),
  password: Yup.string().required('Password is required'),
});

interface LogIntoMtgCbFunction {
  (username: string, password: string): any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

export const logIntoMtgCb: LogIntoMtgCbFunction = async (username, password) => {
  const response = {
    data: null,
    error: null,
  };

  // Before the real logIn, we migrate the user's account from the legacy experience if relevant
  const resultFromMigratingAccount = await migrateLegacyAccount(username, password);
  const authResponse = resultFromMigratingAccount?.data?.data?.authenticateUserWithPasswordLegacy;
  if (!authResponse?.readyForLogin) {
    response.error = authResponse?.reason || authResponse?.error || 'An unknown error has occured';
    return response;
  }

  const result = await logIn(username, password);

  const logInWasSuccessful = result?.data?.data?.authenticateUserWithPassword?.token != null; // TODO: The caller shouldn't have to know this data shape
  if (logInWasSuccessful) {
    response.data = {
      username: result?.data?.data?.authenticateUserWithPassword?.item?.username ?? 'Unknown',
      id: result?.data?.data?.authenticateUserWithPassword?.item?.id,
    };
  } else {
    const errorMessage = result?.data?.data?.authenticateUserWithPassword?.message;
    const invalidLogin = errorMessage?.match(/Authentication failed/);
    const isApiFailure = result?.status !== 200;
    if (invalidLogin) {
      response.error =
        'Invalid username or password. Please try again. Note your username is probably not your email address and should exactly match case-sensitivity.';
    } else if (isApiFailure) {
      response.error = 'There was a problem trying to login. Please try again in a moment.';
    } else {
      response.error = 'There was an unknown problem. Please try again.';
    }
  }

  return response;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export const convertQueryToString = (query: string | string[]): string => {
  if (Array.isArray(query)) {
    return JSON.stringify(query);
  }

  return query;
};

export const Login: React.FC = () => {
  const router = useRouter();
  const { isAuthenticated, isCheckingAuth, setUser, user } = useAuthentication();

  if (isCheckingAuth) {
    return <></>;
  }

  if (isAuthenticated) {
    router.push(`/collections/${user?.id || ''}`);
    return <></>;
  }

  return (
    <LoginWrapper>
      <LoginIcon>
        <LockOutlinedIcon />
      </LoginIcon>
      <Typography component="h1" variant="h5">
        Log In
      </Typography>
      <Formik
        initialValues={{ username: '', password: '' }}
        validationSchema={LoginSchema}
        onSubmit={async (values, { setSubmitting, setStatus }) => {
          setSubmitting(true);
          setStatus(null);
          const result = await logIntoMtgCb(values.username, values.password);
          setStatus(result.error);
          const loginWasSuccessful = result.data?.id;
          if (loginWasSuccessful) {
            const getUserDataResult = await getUser();
            setStatus(getUserDataResult.error);
            const gettingAdditionalUserDataWasSuccessful = getUserDataResult?.data?.data?.authenticatedUser;
            if (gettingAdditionalUserDataWasSuccessful) {
              setUser(getUserDataResult?.data?.data?.authenticatedUser);
            }
          }
        }}
      >
        {({ isSubmitting, errors, touched, status }) => (
          <LoginForm noValidate id="login-form">
            <Field id="username" name="username">
              {({ field }) => (
                <TextField
                  label="Username"
                  type="text"
                  variant="outlined"
                  margin="normal"
                  required
                  fullWidth
                  autoComplete="nickname"
                  autoFocus
                  error={touched.username && Boolean(errors.username)}
                  helperText={touched.username ? errors?.username : 'Note that this is case sensitive!'}
                  inputProps={{ maxLength: 255 }}
                  {...field}
                />
              )}
            </Field>

            <Field name="password" id="password">
              {({ field }) => (
                <TextField
                  label="Password"
                  variant="outlined"
                  margin="normal"
                  required
                  fullWidth
                  autoComplete="current-password"
                  type="password"
                  error={touched.password && Boolean(errors.password)}
                  helperText={touched.password ? errors.password : ''}
                  inputProps={{ maxLength: 255 }}
                  {...field}
                />
              )}
            </Field>

            <Box>
              <FormHelperText error={Boolean(status)}>{status}</FormHelperText>
            </Box>

            <SubmitButtonWrapper>
              <Button type="submit" fullWidth variant="contained" color="primary" isSubmitting={isSubmitting}>
                Log In
              </Button>
            </SubmitButtonWrapper>
            <Grid container>
              <Grid item xs>
                <Link href="#" variant="body2">
                  Forgot password?
                  <br />
                  (In Progress)
                </Link>
              </Grid>
              <Grid item>
                <Link href="/signup" variant="body2">
                  Don't have an account? Sign Up
                </Link>
              </Grid>
            </Grid>
          </LoginForm>
        )}
      </Formik>
    </LoginWrapper>
  );
};
