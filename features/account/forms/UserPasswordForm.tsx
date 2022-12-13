import Box from '@material-ui/core/Box';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import CardHeader from '@material-ui/core/CardHeader';
import Divider from '@material-ui/core/Divider';
import FormHelperText from '@material-ui/core/FormHelperText';
import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import { Field, Form, Formik } from 'formik';
import { useSnackbar } from 'notistack';
import { Dispatch, SetStateAction } from 'react';
import styled from 'styled-components';
import * as Yup from 'yup';
import { User } from '../../../auth/AuthenticationProvider';
import Button from '../../../components/Button';
import { updatePassword } from '../../../network/features/account';

interface UserPasswordFormProps {
  user: User;
  setUser: Dispatch<SetStateAction<User>>;
}

const UserPasswordForm: React.FC<UserPasswordFormProps> = ({ user, setUser }) => {
  const { enqueueSnackbar } = useSnackbar();

  return (
    <Formik
      initialValues={{ password: '', passwordConfirmation: '' }}
      validationSchema={UpdatePasswordSchema}
      onSubmit={async (values, { setSubmitting, setStatus }) => {
        setSubmitting(true);
        setStatus(null);
        const result = await updateUserPassword(user.id, values.password, values.passwordConfirmation);
        setStatus(result.error);
        const userUpdateWasSuccessful = result?.data?.id;
        if (userUpdateWasSuccessful) {
          setUser(result.data);
          enqueueSnackbar('Your password has been updated.', {
            variant: 'success',
            anchorOrigin: { horizontal: 'right', vertical: 'bottom' },
          });
        }
      }}
    >
      {({ isSubmitting, errors, touched, status, dirty, isValid }) => (
        <Card>
          <UpdatePasswordForm noValidate id="update-password-form">
            <CardHeader title="Update Password" titleTypographyProps={{ variant: 'h6' }} />
            <Divider />
            <CardContent>
              <Grid container spacing={2}>
                <Grid item xs={12} md={6}>
                  <Field name="password" id="password">
                    {({ field }) => (
                      <TextField
                        label="New Password"
                        variant="outlined"
                        margin="normal"
                        required
                        fullWidth
                        autoComplete="new-password"
                        type="password"
                        error={touched.password && Boolean(errors.password)}
                        helperText={touched.password ? errors.password : ''}
                        inputProps={{ maxLength: 255 }}
                        {...field}
                      />
                    )}
                  </Field>
                </Grid>
                <Grid item xs={12} md={6}>
                  <Field name="passwordConfirmation" id="passwordConfirmation">
                    {({ field }) => (
                      <TextField
                        label="Confirm New Password"
                        variant="outlined"
                        margin="normal"
                        required
                        fullWidth
                        autoComplete="new-password"
                        type="password"
                        error={touched.passwordConfirmation && Boolean(errors.passwordConfirmation)}
                        helperText={touched.passwordConfirmation ? errors.passwordConfirmation : ''}
                        inputProps={{ maxLength: 255 }}
                        {...field}
                      />
                    )}
                  </Field>
                </Grid>
              </Grid>
              <Box>
                <FormHelperText error={Boolean(status)}>{status}</FormHelperText>
              </Box>
            </CardContent>
            <Divider />
            <CardActionsWrapper>
              <Button type="submit" fullWidth variant="contained" color="primary" isSubmitting={isSubmitting} disabled={!dirty || !isValid}>
                Save Changes
              </Button>
            </CardActionsWrapper>
          </UpdatePasswordForm>
        </Card>
      )}
    </Formik>
  );
};

const UpdatePasswordSchema = Yup.object().shape({
  password: Yup.string().min(8, 'Password must be at least eight characters long').required('Password is required'),
  passwordConfirmation: Yup.string()
    .min(8, 'Password confirmation must be at least eight characters long')
    .required('Password confirmation is required')
    .oneOf([Yup.ref('password'), null], 'Passwords must match'),
});

const updateUserPassword = async (id, password, passwordConfirmation) => {
  const response = {
    data: null,
    error: null,
  };

  if (password !== passwordConfirmation) {
    response.error = 'Passwords must match';
    return response;
  }

  const result = await updatePassword(id, password);

  const userUpdateWasSuccessful = result.data?.data?.updateUser?.id;
  if (userUpdateWasSuccessful) {
    response.data = result.data.data.updateUser;
  } else {
    const isApiFailure = result?.status !== 200;
    if (isApiFailure) {
      response.error = 'There was a problem trying to update your password. Please try again in a moment.';
    } else {
      response.error = 'There was an unknown problem. Please try again.';
    }
  }
  return response;
};

const UpdatePasswordForm = styled(Form)(() => ({
  width: '100%',
}));

// TODO: Minor code duplication here
const CardActionsWrapper = styled(CardActions)(({ theme }) => ({
  margin: theme.spacing(1),
  display: 'flex',
  justifyContent: 'flex-end',
}));

export default UserPasswordForm;
