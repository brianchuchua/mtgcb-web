import { Dispatch, SetStateAction } from 'react';
import styled from 'styled-components';
import { Form, Formik, Field } from 'formik';
import * as Yup from 'yup';
import { useSnackbar } from 'notistack';
import Box from '@material-ui/core/Box';
import Card from '@material-ui/core/Card';
import CardActions from '@material-ui/core/CardActions';
import CardContent from '@material-ui/core/CardContent';
import CardHeader from '@material-ui/core/CardHeader';
import Divider from '@material-ui/core/Divider';
import FormHelperText from '@material-ui/core/FormHelperText';
import Grid from '@material-ui/core/Grid';
import TextField from '@material-ui/core/TextField';
import Button from '../../../components/Button';
import { User } from '../../../auth/AuthenticationProvider';
import { updateUser } from '../../../network/features/account';

interface UserProfileFormProps {
  user: User;
  setUser: Dispatch<SetStateAction<User>>;
}

const UserProfileForm: React.FC<UserProfileFormProps> = ({ user, setUser }) => {
  const { enqueueSnackbar } = useSnackbar();

  return (
    <Formik
      initialValues={{ username: user.username, email: user.email }}
      validationSchema={UpdateUserSchema}
      onSubmit={async (values, { setSubmitting, setStatus }) => {
        setSubmitting(true);
        setStatus(null);
        const result = await updateUserProfile(user.id, values.username, values.email);
        setStatus(result.error);
        const userUpdateWasSuccessful = result?.data?.id;
        if (userUpdateWasSuccessful) {
          setUser(result.data);
          enqueueSnackbar('Your profile has been updated.', { variant: 'success' });
        }
      }}
    >
      {({ isSubmitting, errors, touched, status, dirty, isValid }) => (
        <Card>
          <UpdateProfileForm noValidate id="update-profile-form">
            <CardHeader title="Update Profile" titleTypographyProps={{ variant: 'h6' }} />
            <Divider />
            <CardContent>
              <Grid container spacing={2}>
                <Grid item xs={12} md={6}>
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
                        error={touched.username && Boolean(errors.username)}
                        helperText={touched.username ? errors.username : ''}
                        inputProps={{ maxLength: 255 }}
                        {...field}
                      />
                    )}
                  </Field>
                </Grid>
                <Grid item xs={12} md={6}>
                  <Field id="email" name="email">
                    {({ field }) => (
                      <TextField
                        label="Email"
                        type="text"
                        variant="outlined"
                        margin="normal"
                        required
                        fullWidth
                        autoComplete="email"
                        error={touched.email && Boolean(errors.email)}
                        helperText={touched.email ? errors.email : ''}
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
          </UpdateProfileForm>
        </Card>
      )}
    </Formik>
  );
};

const UpdateUserSchema = Yup.object().shape({
  username: Yup.string().required('Username is required'),
  email: Yup.string().required('Email is required').email('Valid email is required'),
});

const updateUserProfile = async (id, username, email) => {
  const response = {
    data: null,
    error: null,
  };

  const result = await updateUser(id, username, email);

  const userUpdateWasSuccessful = result.data?.data?.updateUser?.id;
  if (userUpdateWasSuccessful) {
    response.data = result.data.data.updateUser;
  } else {
    const errorMessage = result?.data?.errors?.[0]?.message;
    const isDuplicateUsername = errorMessage?.match(/unique constraint "user_username_unique"/);
    const isApiFailure = result?.status !== 200;
    if (isDuplicateUsername) {
      response.error = 'That username is already in use. Please choose another.';
    } else if (isApiFailure) {
      response.error = 'There was a problem trying to update your profile. Please try again in a moment.';
    } else {
      response.error = 'There was an unknown problem. Please try again.';
    }
  }
  return response;
};

const UpdateProfileForm = styled(Form)(() => ({
  width: '100%',
}));

// TODO: Minor code duplication here
const CardActionsWrapper = styled(CardActions)(({ theme }) => ({
  margin: theme.spacing(1),
  display: 'flex',
  justifyContent: 'flex-end',
}));

export default UserProfileForm;
