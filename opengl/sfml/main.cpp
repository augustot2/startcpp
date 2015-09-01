#include <SFML/Window.hpp>

int main()
{
    sf::ContextSettings settings;
settings.depthBits = 24;
settings.stencilBits = 8;
settings.antialiasingLevel = 2; // Optional

sf::Window window(sf::VideoMode(800, 600), "OpenGL", sf::Style::Close, settings);
    return 0;
}