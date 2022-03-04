import Typography from '@material-ui/core/Typography';
import { ResponsiveContainer } from '../components/layout/ResponsiveContainer';

const HomePage: React.FC = () => (
  <ResponsiveContainer maxWidth="xl">
    <Typography variant="h3" component="h1" align="center">
      MTG CB Limited Edition Alpha - Build v0.35.1 👷‍♂️
    </Typography>
    <Typography paragraph align="center">
      <em>Last updated: 2022-03-03</em>
    </Typography>
    <br />
    <br />
    <Typography variant="h5" component="h2">
      Introduction
    </Typography>
    <br />
    <Typography paragraph>
      Thank you for joining the Limited Edition Alpha! This is a very early look at the alpha build, exclusive to patrons, which is updated
      (and broken 🚧) regularly. Your feedback is highly valued and appreciated!
    </Typography>
    <Typography variant="h5" component="h2">
      Getting Started
    </Typography>
    <Typography paragraph>
      <ul>
        <li>
          Check out the <u>Browse</u> section on the left, even if you're not logged in, to browse every card and set in Magic with a brand
          new set of filters and viewing options.
        </li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Coming Up Next
    </Typography>
    <Typography paragraph>
      <ul>
        <li>Add Cards page</li>
        <li>Database data fixes -- sets missing release dates, cards missing sets, etc.</li>
        <li>Collection-based filters and sorting</li>
        <li>The Great Subset Split™ and subset interfaces</li>
        <li>Bug fixes</li>
      </ul>
    </Typography>
    <Typography variant="h5" component="h2">
      Roadmap
    </Typography>
    <Typography paragraph>
      <ul>
        <li>⏳ Limited Edition Alpha (In Progress): Feature parity with the current MTG CB. This is being moved one piece at a time.</li>
        <li>
          Limited Edition Beta (March): As many of the new MTG CB features that can be squeezed in here (Tokens, Custom Collection Goals,
          and Foreign Cards).
        </li>
        <li>Unlimited Edition (June): Full release of the new MTG CB.</li>
        <li>
          Combo Winter (New): I need to invest some time in upgrading the libraries used by the new MTG CB. I'll do this after the main
          release.
        </li>
        <li>Revised Edition (TBD): The v1.1 update adding a single new requested feature.</li>
        <li>
          Future Updates (TBD): Future updates will follow the same pattern of v1.2, v1.3, and so on, each adding a single new feature in
          focused iterations.
        </li>
      </ul>
    </Typography>
    This page will eventually be a snazzy homepage, but for now, you can use it to see what's new in the latest build and what's coming up
    next. Feel free to send me any feedback via Discord or Patreon! <em>There will be bugs. 🐛</em>
  </ResponsiveContainer>
);

export default HomePage;
