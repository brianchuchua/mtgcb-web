import CssBaseline from '@material-ui/core/CssBaseline';
import { StylesProvider, ThemeProvider as MuiThemeProvider } from '@material-ui/core/styles';
import 'fontsource-roboto';
import { AppProps } from 'next/app';
import { SnackbarProvider } from 'notistack';
import { useEffect } from 'react';
import { Provider as ReduxProvider } from 'react-redux';
import { ThemeProvider } from 'styled-components';
import { AuthenticationProvider } from '../auth/AuthenticationProvider';
import Layout from '../components/layout/Layout';
import GlobalStyle from '../components/styles/GlobalStyle';
import store from '../redux/store';
import darkTheme from '../themes/darkTheme';

const MtgCbWebApp = ({ Component, pageProps }: AppProps): JSX.Element => {
  useEffect(() => {
    const jssStyles = document.querySelector('#jss-server-side');
    if (jssStyles) {
      jssStyles.parentElement.removeChild(jssStyles);
    }
  }, []);

  if (process.browser) {
    console.log(
      `%c
      ███╗   ███╗████████╗ ██████╗      ██████╗██████╗ 
      ████╗ ████║╚══██╔══╝██╔════╝     ██╔════╝██╔══██╗
      ██╔████╔██║   ██║   ██║  ███╗    ██║     ██████╔╝
      ██║╚██╔╝██║   ██║   ██║   ██║    ██║     ██╔══██╗
      ██║ ╚═╝ ██║   ██║   ╚██████╔╝    ╚██████╗██████╔╝
      ╚═╝     ╚═╝   ╚═╝    ╚═════╝      ╚═════╝╚═════╝     
Hello fellow dev! Feel free to check out MTG CB on GitHub. :)
         https://github.com/brianchuchua/mtgcb-web         
`,
      'font-family:monospace;color:#1976d2;font-size:12px;'
    );
  }

  return (
    <MuiThemeProvider theme={darkTheme}>
      <SnackbarProvider maxSnack={3}>
        <StylesProvider injectFirst>
          <ThemeProvider theme={darkTheme}>
            <CssBaseline />
            <GlobalStyle />
            <ReduxProvider store={store}>
              <AuthenticationProvider>
                <Layout>
                  <Component {...pageProps} />
                </Layout>
              </AuthenticationProvider>
            </ReduxProvider>
          </ThemeProvider>
        </StylesProvider>
      </SnackbarProvider>
    </MuiThemeProvider>
  );
};

export default MtgCbWebApp;
